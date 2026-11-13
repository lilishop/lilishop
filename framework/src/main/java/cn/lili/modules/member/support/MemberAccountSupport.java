package cn.lili.modules.member.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.support.LoginFailLimitSupport;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.mapper.MemberMapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.enums.SqlMethod;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import org.apache.ibatis.binding.MapperMethod;
import org.apache.ibatis.logging.Log;
import org.apache.ibatis.logging.LogFactory;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import java.util.List;

/**
 * 会员账号基础查询、密码重置、店铺状态和登录态维护，供会员与员工服务共用，避免服务循环依赖。
 * 员工归属、店主限制等业务资格由入口服务校验；缓存撤销不随数据库事务回滚。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class MemberAccountSupport {

    private static final Log BATCH_LOG = LogFactory.getLog(MemberAccountSupport.class);
    private static final int BATCH_SIZE = 1000;

    private final MemberMapper mapper;
    private final Cache cache;
    private final LoginFailLimitSupport loginFailLimitSupport;

    /**
     * 注入会员存储和账号缓存能力，不依赖会员、员工或店铺业务服务。
     *
     * @param mapper 会员存储
     * @param cache 登录态缓存
     * @param loginFailLimitSupport 登录失败限制维护
     */
    public MemberAccountSupport(MemberMapper mapper, Cache cache, LoginFailLimitSupport loginFailLimitSupport) {
        this.mapper = mapper;
        this.cache = cache;
        this.loginFailLimitSupport = loginFailLimitSupport;
    }

    /**
     * 按主键读取会员，沿用存储层逻辑删除规则，不额外限制账号状态。
     *
     * @param memberId 会员主键
     * @return 会员，不存在时为null
     */
    public Member getById(String memberId) {
        return mapper.selectById(memberId);
    }

    /**
     * 按手机号精确读取会员，保留多条匹配时抛错的原有行为。
     *
     * @param mobile 手机号
     * @return 会员，不存在时为null
     */
    public Member findByMobile(String mobile) {
        return mapper.selectOne(new QueryWrapper<Member>().eq("mobile", mobile));
    }

    /**
     * 管理端直接开店后，只更新会员的开店标志及店铺主键，不改动其他账号字段或登录态。
     * 数据库写入沿用创建店铺的调用方事务。
     *
     * @param memberId 店主会员主键
     * @param storeId 新建店铺主键
     */
    public void markStoreOpened(String memberId, String storeId) {
        mapper.update(null, new LambdaUpdateWrapper<Member>()
                .eq(Member::getId, memberId)
                .set(Member::getHaveStore, true)
                .set(Member::getStoreId, storeId));
    }

    /**
     * 开店审核通过后保存调用方已回填店铺信息的店主实体，沿用原有按主键整实体更新及字段策略。
     * 不增加登录态撤销或其他账号校验，数据库写入参与审核入口事务。
     *
     * @param member 已设置开店标志和店铺主键的店主会员实体
     */
    public void updateStoreOwner(Member member) {
        mapper.updateById(member);
    }

    /**
     * 批量重置为原有默认密码编码：对123456进行MD5后再做BCrypt，不改变历史登录口径。
     * 数据库更新沿用调用方事务，随后撤销会员及商家登录态并清理会员端登录失败记录。
     * 缓存清理保持立即执行，即使外层事务回滚也不恢复已撤销的令牌。
     *
     * @param ids 会员主键列表，沿用入口校验及原有批量更新范围
     */
    public void resetPassword(List<String> ids) {
        String password = new BCryptPasswordEncoder().encode(StringUtils.md5("123456"));
        LambdaUpdateWrapper<Member> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.in(Member::getId, ids);
        updateWrapper.set(Member::getPassword, password);
        mapper.update(null, updateWrapper);
        revokeMemberTokens(ids);
        if (ids != null && !ids.isEmpty()) {
            List<Member> members = mapper.selectByIds(ids);
            if (members != null) {
                members.forEach(member -> loginFailLimitSupport.clearLoginFail(UserEnums.MEMBER, member.getUsername()));
            }
        }
    }

    /**
     * 读取会员实体后批量更新开店状态，保留原有整实体更新及空字段持久化策略。
     * 有店铺时设置传入店铺ID，否则将实体上的店铺ID置空；空主键列表直接跳过。
     * 批量写入加入调用方事务，无事务时开启事务，不额外撤销登录态。
     *
     * @param haveStore 是否拥有店铺
     * @param storeId 有店铺时使用的店铺主键
     * @param memberIds 会员主键列表，可为null
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateHaveShop(Boolean haveStore, String storeId, List<String> memberIds) {
        if (memberIds == null || memberIds.isEmpty()) {
            return;
        }
        List<Member> members = mapper.selectByIds(memberIds);
        if (members != null && !members.isEmpty()) {
            members.forEach(member -> {
                member.setHaveStore(haveStore);
                member.setStoreId(Boolean.TRUE.equals(haveStore) ? storeId : null);
            });
            // 沿用ServiceImpl.updateBatchById的SQL语句、参数和批次大小，保持Spring事务内批量执行。
            String statement = SqlHelper.getSqlStatement(MemberMapper.class, SqlMethod.UPDATE_BY_ID);
            SqlHelper.executeBatch(Member.class, BATCH_LOG, members, BATCH_SIZE, (session, member) -> {
                MapperMethod.ParamMap<Member> params = new MapperMethod.ParamMap<>();
                params.put(Constants.ENTITY, member);
                session.update(statement, params);
            });
        }
    }

    /**
     * 按会员主键撤销买家及商家两端的访问令牌和刷新令牌，沿用原有缓存前缀和执行顺序。
     *
     * @param memberIds 会员主键列表，null或空列表不执行清理
     */
    public void revokeMemberTokens(List<String> memberIds) {
        if (memberIds != null && !memberIds.isEmpty()) {
            memberIds.forEach(memberId -> {
                cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
                cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
                cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.STORE, memberId));
                cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.STORE, memberId));
            });
        }
    }
}
