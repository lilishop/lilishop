package cn.lili.modules.store.service;

import cn.hutool.core.date.DateTime;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.entity.vos.BillListVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;

import java.util.Date;

/**
 * 结算单业务层
 *
 * @author Chopper
 * @date 2020/11/17 4:28 下午
 */
@CacheConfig(cacheNames = "bill")
public interface BillService extends IService<Bill> {

    /**
     * 生成结算单
     *
     * @param storeId   商家ID
     * @param startTime 开始时间
     * @param endTime   结束时间
     */
    void createBill(String storeId, Date startTime, DateTime endTime);


    /**
     * 立即结算
     * 用于关闭商家，立即结算使用
     *
     * @param storeId
     * @param endTime 结束时间
     */
    void immediatelyBill(String storeId, Long endTime);

    /**
     * 根据结算单ID获取商家流水
     *
     * @param pageVO 分页
     * @param id     结算单ID
     * @param type   类型
     * @return 商家流水
     */
    IPage<StoreFlow> getStoreFlow(String id, String type, PageVO pageVO);

    /**
     * 根据结算单ID获取商家流水
     *
     * @param pageVO 分页
     * @param id     结算单ID
     * @return 商家流水
     */
    IPage<StoreFlow> getDistributionFlow(String id, PageVO pageVO);

    /**
     * 获取结算单分页
     *
     * @param billSearchParams 结算单搜索条件
     * @return 结算单分页
     */
    IPage<BillListVO> billPage(BillSearchParams billSearchParams);

    /**
     * 商家核对结算单
     *
     * @param id 结算单ID
     * @return 操作状态
     */
    boolean check(String id);

    /**
     * 平台结算
     *
     * @param id 结算单ID
     * @return 操作状态
     */
    boolean complete(String id);

    /**
     * 商家待结算数量
     *
     * @param billStatusEnum 结算单类型
     * @return 待结算商家数量
     */
    Integer billNum(BillStatusEnum billStatusEnum);

}