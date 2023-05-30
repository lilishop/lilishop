package cn.lili.modules.store.service;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.dto.*;
import cn.lili.modules.store.entity.vos.StoreBasicInfoVO;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.store.entity.vos.StoreManagementCategoryVO;
import cn.lili.modules.store.entity.vos.StoreOtherVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 店铺详细业务层
 *
 * @author pikachu
 * @since 2020-03-07 09:24:33
 */
public interface StoreDetailService extends IService<StoreDetail> {
    /**
     * 根据店铺ID获取店铺信息VO
     *
     * @param storeId 店铺ID
     * @return 店铺信息VO
     */
    StoreDetailVO getStoreDetailVO(String storeId);

    /**
     * 根据会员ID获取店铺信息VO
     *
     * @param memberId 会员ID
     * @return 店铺信息VO
     */
    StoreDetailVO getStoreDetailVOByMemberId(String memberId);

    /**
     * 根据店铺ID获取店铺信息DO
     *
     * @param storeId 店铺ID
     * @return 店铺信息DO
     */
    StoreDetail getStoreDetail(String storeId);

    /**
     * 修改商家设置
     *
     * @param storeSettingDTO 店铺设置信息
     * @return 店铺详情
     */
    Boolean editStoreSetting(StoreSettingDTO storeSettingDTO);

    /**
     * 获取店铺基本信息
     * 用于前端店铺信息展示
     *
     * @param storeId 店铺ID
     * @return 店铺基本信息
     */
    StoreBasicInfoVO getStoreBasicInfoDTO(String storeId);

    /**
     * 获取当前登录店铺售后收件地址
     *
     * @return 店铺售后收件地址
     */
    StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO();

    /**
     * 获取某一个店铺的退货收件地址信息
     *
     * @param id 店铺ID
     * @return 店铺售后收件地址
     */
    StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO(String id);

    /**
     * 修改当前登录店铺售后收件地址
     *
     * @param storeAfterSaleAddressDTO 店铺售后DTO
     * @return 店铺售后收件地址
     */
    boolean editStoreAfterSaleAddressDTO(StoreAfterSaleAddressDTO storeAfterSaleAddressDTO);



    /**
     * 修改店铺库存预警数量
     *
     * @param stockWarning 库存预警数量
     * @return 操作状态
     */
    boolean updateStockWarning(Integer stockWarning);

    /**
     * 获取店铺经营范围
     *
     * @param storeId 店铺ID
     * @return 店铺经营范围
     */
    List<StoreManagementCategoryVO> goodsManagementCategory(String storeId);

    /**
     * 获取店铺其他信息
     *
     * @param storeId 店铺ID
     * @return 店铺其他信息
     */
    StoreOtherVO getStoreOtherVO(String storeId);

    /**
     * 更新店铺内所有商品信息
     *
     * @param store 店铺信息
     */
    void updateStoreGoodsInfo(Store store);

    /**
     * 修改店铺udesk字段设置
     *
     * @param merchantEuid 店铺客服信息
     */
    Boolean editMerchantEuid(String merchantEuid);

    /**
     * 获取待结算店铺列表
     *
     * @param day 结算日
     * @return 待结算店铺列表
     */
    List<StoreSettlementDay> getSettlementStore(int day);

    /**
     * 修改店铺的结算日
     *
     * @param storeId  店铺ID
     * @param dateTime 结算日
     */
    void updateSettlementDay(String storeId, DateTime dateTime);


    /**
     * 获取当前登录店铺发件地址
     *
     * @return 店铺售后发件地址
     */
    StoreDeliverGoodsAddressDTO getStoreDeliverGoodsAddressDto();

    /**
     * 获取某一个店铺的发货寄件地址信息
     *
     * @param id 店铺ID
     * @return 店铺发件地址
     */
    StoreDeliverGoodsAddressDTO getStoreDeliverGoodsAddressDto(String id);

    /**
     * 修改当前登录店铺发件地址
     *
     * @param storeDeliverGoodsAddressDto 店铺发货信息DTO
     * @return 店铺售后发件地址
     */
    boolean editStoreDeliverGoodsAddressDTO(StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDto);
}