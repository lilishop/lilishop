package cn.lili.modules.store.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dto.CollectionDTO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.dto.*;
import cn.lili.modules.store.entity.vos.StoreSearchParams;
import cn.lili.modules.store.entity.vos.StoreVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 店铺业务层
 *
 * @author pikachu
 * @since 2020/11/18 11:45 上午
 */
public interface StoreService extends IService<Store> {

    /**
     * 分页条件查询
     * 用于展示店铺列表
     *
     * @param entity
     * @return
     */
    IPage<StoreVO> findByConditionPage(StoreSearchParams entity, PageVO page);

    /**
     * 获取当前登录店铺信息
     *
     * @return 店铺信息DO
     */
    StoreVO getStoreDetail();

    /**
     * 增加店铺
     * 用于后台添加店铺
     *
     * @param adminStoreApplyDTO 后台添加店铺信息
     * @return 店铺
     */
    Store add(AdminStoreApplyDTO adminStoreApplyDTO);

    /**
     * 编辑店铺
     *
     * @param storeEditDTO 店铺修改信息
     * @return 店铺
     */
    Store edit(StoreEditDTO storeEditDTO);

    /**
     * 审核店铺
     *
     * @param id     店铺ID
     * @param passed 审核结果
     * @return 操作结果
     */
    boolean audit(String id, Integer passed);

    /**
     * 关闭店铺
     *
     * @param id 店铺ID
     * @return 店铺
     */
    boolean disable(String id);

    /**
     * 开启店铺
     *
     * @param id 店铺ID
     * @return 操作状态
     */
    boolean enable(String id);

    /**
     * 申请店铺第一步
     * 设置店铺公司信息，如果没有店铺新建店铺
     *
     * @param storeCompanyDTO 店铺公司信息
     * @return 店铺
     */
    boolean applyFirstStep(StoreCompanyDTO storeCompanyDTO);

    /**
     * 申请店铺第二步
     *
     * @param storeBankDTO 店铺银行信息
     * @return 店铺
     */
    boolean applySecondStep(StoreBankDTO storeBankDTO);

    /**
     * 申请店铺第三步
     * 设置店铺信息，经营范围
     *
     * @param storeOtherInfoDTO 店铺其他信息
     * @return 店铺
     */
    boolean applyThirdStep(StoreOtherInfoDTO storeOtherInfoDTO);


    /**
     * 更新店铺商品数量
     *
     * @param storeId 店铺ID
     */
    void updateStoreGoodsNum(String storeId);

    /**
     * 更新店铺收藏数量
     *
     * @param collectionDTO 收藏信息
     */
    void updateStoreCollectionNum(CollectionDTO collectionDTO);
}