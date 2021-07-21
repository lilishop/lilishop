package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.PointsGoodsCategory;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 积分商品分类业务层
 *
 * @author paulG
 * @since 2020/11/18 9:45 上午
 **/
public interface PointsGoodsCategoryService extends IService<PointsGoodsCategory> {

    /**
     * 添加积分商品分类
     *
     * @param pointsGoodsCategory 积分商品分类信息
     * @return 是否添加成功
     */
    boolean addCategory(PointsGoodsCategory pointsGoodsCategory);

    /**
     * 更新积分商品分类
     *
     * @param pointsGoodsCategory 积分商品分类信息
     * @return 是否更新成功
     */
    boolean updateCategory(PointsGoodsCategory pointsGoodsCategory);

    /**
     * 删除积分商品类型
     *
     * @param id 积分商品分类id
     * @return 是否删除成功
     */
    boolean deleteCategory(String id);

    /**
     * 分页获取积分商品类型
     *
     * @param name 类型名称
     * @param page 分页参数
     * @return 积分商品类型分页数据
     */
    IPage<PointsGoodsCategory> getCategoryByPage(String name, PageVO page);

    /**
     * 获取积分商品类型详情
     *
     * @param id 积分商品类型id
     * @return 积分商品类型详情
     */
    PointsGoodsCategory getCategoryDetail(String id);

}
