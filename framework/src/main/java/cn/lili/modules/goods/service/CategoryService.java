package cn.lili.modules.goods.service;


import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 商品分类业务层
 *
 * @author pikachu
 * @since 2020-03-02 16:44:56
 */
public interface CategoryService extends IService<Category> {


    /**
     * 管理端获取所有分类
     * 即获取的对象不管是否删除，都要展示，而且不从缓存获取，保证内容是最新的
     *
     * @param parentId 分类父ID
     * @return 商品分类列表
     */
    List<Category> dbList(String parentId);

    /**
     * 获取分类
     *
     * @param id
     * @return
     */
    Category getCategoryById(String id);

    /**
     * 根据分类id集合获取所有分类根据层级排序
     *
     * @param ids 分类ID集合
     * @return 商品分类列表
     */
    List<Category> listByIdsOrderByLevel(List<String> ids);

    /**
     * 根据分类id集合获取所有分类根据层级排序
     *
     * @param ids 分类ID集合
     * @return 商品分类列表
     */
    List<Map<String, Object>> listMapsByIdsOrderByLevel(List<String> ids, String columns);

    /**
     * 获取分类树
     *
     * @return 分类树
     */
    List<CategoryVO> categoryTree();

    /**
     * 查询所有的分类，父子关系
     *
     * @param parentId 分类父ID
     * @return 所有的分类，父子关系
     */
    List<CategoryVO> listAllChildren(String parentId);

    /**
     * 查询所有的分类，父子关系
     * 数据库获取
     *
     * @return 所有的分类，父子关系
     */
    List<CategoryVO> listAllChildren();

    /**
     * 获取指定分类的分类名称
     *
     * @param ids 指定分类id集合
     * @return 分类名称集合
     */
    List<String> getCategoryNameByIds(List<String> ids);

    /**
     * 获取商品分类list
     *
     * @param category 分类
     * @return 商品分类list
     */
    List<Category> findByAllBySortOrder(Category category);

    /**
     * 添加商品分类
     *
     * @param category 商品分类信息
     * @return 添加结果
     */
    boolean saveCategory(Category category);

    /**
     * 修改商品分类
     *
     * @param category 商品分类信息
     * @return 修改结果
     */
    void updateCategory(Category category);

    /**
     * 批量删除分类
     *
     * @param id 分类ID
     */
    void delete(String id);

    /**
     * 分类状态的更改
     *
     * @param categoryId       商品分类ID
     * @param enableOperations 是否可用
     */
    void updateCategoryStatus(String categoryId, Boolean enableOperations);

    /**
     * 获取商家经营类目
     *
     * @param categories 经营范围
     * @return 分类VO列表
     */
    List<CategoryVO> getStoreCategory(String[] categories);

    /**
     * 获取一级分类列表
     * 用于商家入驻选择
     *
     * @return 分类列表
     */
    List<Category> firstCategory();

}