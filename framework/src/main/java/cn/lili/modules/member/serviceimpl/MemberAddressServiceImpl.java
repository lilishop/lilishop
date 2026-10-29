package cn.lili.modules.member.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.mapper.MemberAddressMapper;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

/**
 * 收货地址业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 9:44 上午
 */
@Service
public class MemberAddressServiceImpl extends ServiceImpl<MemberAddressMapper, MemberAddress> implements MemberAddressService {

    @Autowired
    private MemberExperienceService memberExperienceService;

    @Override
    public IPage<MemberAddress> getAddressByMember(PageVO page, String memberId) {
        return this.page(PageUtil.initPage(page),
                new QueryWrapper<MemberAddress>()
                        .eq("member_id", memberId));

    }

    @Override
    public MemberAddress getMemberAddress(String id) {
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser.getIsSuper() || UserEnums.MANAGER.equals(authUser.getRole())){
            return this.getOne(new QueryWrapper<MemberAddress>().eq("id", id));
        }else{
            return this.getOne(
                    new QueryWrapper<MemberAddress>()
                            .eq("member_id", Objects.requireNonNull(UserContext.getCurrentUser()).getId())
                            .eq("id", id));
        }
    }

    /**
     * 根据地址ID获取当前客户地址信息
     *
     * @return 当前客户的地址信息
     */
    @Override
    public MemberAddress getDefaultMemberAddress() {
        return this.getOne(
                new QueryWrapper<MemberAddress>()
                        .eq("member_id", Objects.requireNonNull(UserContext.getCurrentUser()).getId())
                        .eq("is_default", true));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public MemberAddress saveMemberAddress(MemberAddress memberAddress) {
        //过滤收货地址敏感词
        memberAddress.setName(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getName()));
        memberAddress.setDetail(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getDetail()));
        memberAddress.setAlias(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getAlias()));
        //判断当前地址是否为默认地址，如果为默认需要将其他的地址修改为非默认
        removeDefaultAddress(memberAddress);
        //添加客户地址
        this.save(memberAddress);
        memberExperienceService.grantExperience(memberAddress.getMemberId(), ExperienceRuleEnum.ADD_ADDRESS, memberAddress.getId(), "添加收货地址赠送经验值");

        return memberAddress;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public MemberAddress updateMemberAddress(MemberAddress memberAddress) {
        //过滤收货地址敏感词
        memberAddress.setName(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getName()));
        memberAddress.setDetail(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getDetail()));
        memberAddress.setAlias(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberAddress.getAlias()));
        MemberAddress originalMemberAddress = this.getMemberAddress(memberAddress.getId());
        if (originalMemberAddress != null) {
            if (memberAddress.getIsDefault() == null) {
                memberAddress.setIsDefault(false);
            }
            //判断当前地址是否为默认地址，如果为默认需要将其他的地址修改为非默认
            removeDefaultAddress(memberAddress);
            this.saveOrUpdate(memberAddress);
        }

        return memberAddress;
    }

    @Override
    public boolean removeMemberAddress(String id) {
        return this.remove(new QueryWrapper<MemberAddress>()
                .eq("id", id));
    }

    /**
     * 修改客户默认收件地址
     *
     * @param memberAddress 收件地址
     */
    private void removeDefaultAddress(MemberAddress memberAddress) {
        //如果不是默认地址不需要处理
        if (Boolean.TRUE.equals(memberAddress.getIsDefault())) {
            //将客户的地址修改为非默认地址
            LambdaUpdateWrapper<MemberAddress> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.set(MemberAddress::getIsDefault, false);
            lambdaUpdateWrapper.eq(MemberAddress::getMemberId, memberAddress.getMemberId());
            this.update(lambdaUpdateWrapper);
        }

    }
}
