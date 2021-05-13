package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.vos.FullDiscountSearchParams;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 满优惠业务层
 *
 * @author Chopper
 * @date 2020/8/21
 */
public interface FullDiscountService extends IService<FullDiscount> {

    /**
     * 当前满优惠活动
     *
     * @param storeId 商家编号
     * @return 满优惠活动信息
     */
    FullDiscountVO currentPromotion(String storeId);

    /**
     * 当前满优惠活动
     *
     * @param storeId 商家编号
     * @return 满优惠活动信息
     */
    List<FullDiscountVO> currentPromotion(List<String> storeId);

    /**
     * 添加满优惠活动
     *
     * @param fullDiscountVO 满优惠活动信息
     * @return 满优惠活动
     */
    FullDiscount addFullDiscount(FullDiscountVO fullDiscountVO);

    /**
     * 从mysql中分页获取满优惠列表
     *
     * @param searchParams 参数
     * @param page         分页参数
     * @return 满优惠列表
     */
    IPage<FullDiscount> getFullDiscountByPageFromMysql(FullDiscountSearchParams searchParams, PageVO page);

    /**
     * 从mongo中分页获取满优惠列表
     *
     * @param searchParams    搜索参数
     * @param page            分页参数
     * @return 满优惠列表
     */
    IPage<FullDiscountVO> getFullDiscountByPageFromMongo(FullDiscountSearchParams searchParams, PageVO page);


    /**
     * 修改满优惠活动
     *
     * @param fullDiscountVO 满优惠活动信息
     * @return 满优惠活动
     */
    FullDiscountVO modifyFullDiscount(FullDiscountVO fullDiscountVO);

    /**
     * 获取满优惠活动详情
     *
     * @param id 满优惠KID
     * @return 满优惠活动详情
     */
    FullDiscountVO getFullDiscount(String id);

    /**
     * 删除满优惠获取
     *
     * @param id 满优惠活动编号
     * @return 删除结果
     */
    boolean deleteFullDiscount(String id);

}