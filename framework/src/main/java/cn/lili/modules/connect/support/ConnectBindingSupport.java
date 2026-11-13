package cn.lili.modules.connect.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.mapper.ConnectMapper;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.stereotype.Component;

/**
 * 联合登录绑定记录的基础读写，供会员与联合登录服务共用，避免双方业务服务相互依赖。
 * 只处理原有存储操作，登录注册、绑定资格、Cookie和账号注销规则由调用方维护。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class ConnectBindingSupport {

    private final ConnectMapper mapper;

    /**
     * 注入联合登录绑定存储，不依赖会员或联合登录业务服务。
     *
     * @param mapper 联合登录绑定存储
     */
    public ConnectBindingSupport(ConnectMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 按非空会员主键、联合登录类型和第三方主键组合查询，保留多条匹配时返回首条的原有规则。
     * 不额外校验绑定资格或添加查询条件。
     *
     * @param query 绑定查询条件
     * @return 匹配的绑定记录，不存在时为null
     */
    public Connect queryConnect(ConnectQueryDTO query) {
        LambdaQueryWrapper<Connect> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(CharSequenceUtil.isNotEmpty(query.getUserId()), Connect::getUserId, query.getUserId())
                .eq(CharSequenceUtil.isNotEmpty(query.getUnionType()), Connect::getUnionType, query.getUnionType())
                .eq(CharSequenceUtil.isNotEmpty(query.getUnionId()), Connect::getUnionId, query.getUnionId());
        return mapper.selectOne(wrapper, false);
    }

    /**
     * 保存调用方已判断需要新增的绑定记录，沿用存储层主键与审计字段填充规则及调用方事务。
     *
     * @param connect 待保存的绑定记录
     * @return 插入影响行数大于零时为true
     */
    public boolean save(Connect connect) {
        return mapper.insert(connect) > 0;
    }

    /**
     * 按会员主键删除其全部联合登录绑定，沿用原删除方式及调用方事务。
     * 不额外修改会员、撤销登录态或清理Cookie。
     *
     * @param userId 会员主键
     */
    public void deleteByMemberId(String userId) {
        mapper.delete(new LambdaQueryWrapper<Connect>().eq(Connect::getUserId, userId));
    }
}
