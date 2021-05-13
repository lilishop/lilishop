package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.mapper.MemberAddressMapper;
import cn.lili.modules.promotion.service.MemberAddressService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 收货地址业务层实现
 *
 * @author Chopper
 * @date 2020/11/18 9:44 上午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class MemberAddressServiceImpl extends ServiceImpl<MemberAddressMapper, MemberAddress> implements MemberAddressService {

    @Override
    public IPage<MemberAddress> getAddressByMember(PageVO page,String memberId) {
        return this.page(PageUtil.initPage(page),
                new QueryWrapper<MemberAddress>()
                        .eq("member_id", memberId));

    }

    @Override
    public MemberAddress getMemberAddress(String id) {
        return this.getOne(
                new QueryWrapper<MemberAddress>()
                        .eq("member_id", UserContext.getCurrentUser().getId())
                        .eq("id", id));
    }

    /**
     * 根据地址ID获取当前会员地址信息
     *
     * @return 当前会员的地址信息
     */
    @Override
    public MemberAddress getDefaultMemberAddress() {
        return this.getOne(
                new QueryWrapper<MemberAddress>()
                        .eq("member_id", UserContext.getCurrentUser().getId())
                        .eq("is_default", true));
    }

    @Override
    public MemberAddress saveMemberAddress(MemberAddress memberAddress) {
        //判断当前地址是否为默认地址，如果为默认需要将其他的地址修改为非默认
        updateDefaultShippingAddress(memberAddress);
        //添加会员地址
        this.save(memberAddress);

        return memberAddress;
    }

    @Override
    public MemberAddress updateMemberAddress(MemberAddress memberAddress) {
        //判断当前地址是否为默认地址，如果为默认需要将其他的地址修改为非默认
        updateDefaultShippingAddress(memberAddress);
        //修改会员地址
        this.update(memberAddress,
                new QueryWrapper<MemberAddress>()
                        .eq("id", memberAddress.getId()));
        return memberAddress;
    }

    @Override
    public boolean removeMemberAddress(String id) {
        return this.remove(new QueryWrapper<MemberAddress>()
                .eq("id", id));
    }

    /**
     * 修改会员默认收件地址
     *
     * @param shippingAddress 收件地址
     */
    private void updateDefaultShippingAddress(MemberAddress shippingAddress) {
        //校验此地址是否为第一个会员地址  如果是默认是会员默认地址
        List<MemberAddress> list = this.baseMapper.selectList(new QueryWrapper<MemberAddress>().eq("member_id", shippingAddress.getMemberId()));
        if (list.size() == 1) {
            shippingAddress.setIsDefault(true);
        }
        //如果不是默认地址不需要处理
        if (shippingAddress.getIsDefault()) {
            //将会员的地址修改为非默认地址
            LambdaUpdateWrapper<MemberAddress> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.set(MemberAddress::getIsDefault, false);
            lambdaUpdateWrapper.eq(MemberAddress::getMemberId, shippingAddress.getMemberId());
            this.update(lambdaUpdateWrapper);
        }

    }
}