package cn.lili.modules.store.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.store.entity.dos.StoreAddress;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 店铺地址（自提点）详细业务层
 *
 * @author Bulbasaur
 * @since 2020-03-07 09:24:33
 */
public interface StoreAddressService extends IService<StoreAddress> {

    /**
     * 获取当前商家的自提点列表
     *
     * @param pageVo 分页
     * @return 自提点列表
     */
    IPage<StoreAddress> getStoreAddress(String storeId, PageVO pageVo);

    /**
     * 添加商家自提点
     *
     * @param storeAddress 自提点
     * @return 自提点
     */
    StoreAddress addStoreAddress(String storeId, StoreAddress storeAddress);

    /**
     * 修改商家自提点
     *
     * @param storeAddress 自提点
     * @return 自提点
     */
    StoreAddress editStoreAddress(String storeId, StoreAddress storeAddress);

    /**
     * 删除商家自提点
     *
     * @param id 自提点ID
     * @return 操作状态
     */
    Boolean removeStoreAddress(String id);

}