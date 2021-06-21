package cn.lili.modules.store.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.store.entity.dos.StoreAddress;
import cn.lili.modules.store.mapper.StoreAddressMapper;
import cn.lili.modules.store.service.StoreAddressService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 店铺地址（自提点）业务层实现
 *
 * @author Bulbasaur
 * @date 2020/11/22 16:00
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StoreAddressServiceImpl extends ServiceImpl<StoreAddressMapper, StoreAddress> implements StoreAddressService {

    @Override
    public IPage<StoreAddress> getStoreAddress(PageVO pageVo) {
        //获取当前登录商家账号
        LambdaQueryWrapper<StoreAddress> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(StoreAddress::getStoreId, UserContext.getCurrentUser().getStoreId());
        return this.page(PageUtil.initPage(pageVo), lambdaQueryWrapper);
    }

    @Override
    public StoreAddress addStoreAddress(StoreAddress storeAddress) {
        //获取当前登录商家账号
        storeAddress.setStoreId(UserContext.getCurrentUser().getStoreId());
        //添加自提点
        this.save(storeAddress);
        return storeAddress;
    }

    @Override
    public StoreAddress editStoreAddress(StoreAddress storeAddress) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        storeAddress.setStoreId(tokenUser.getId());
        //添加自提点
        this.updateById(storeAddress);
        return storeAddress;
    }

    @Override
    public Boolean removeStoreAddress(String id) {
        return this.removeById(id);
    }
}