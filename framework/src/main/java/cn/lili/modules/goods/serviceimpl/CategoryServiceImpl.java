package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.mapper.CategoryMapper;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryParameterGroupService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.CategorySpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;


/**
 * 商品分类业务层实现
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
public class CategoryServiceImpl extends ServiceImpl<CategoryMapper, Category> implements CategoryService {

    private static final String DELETE_FLAG_COLUMN = "delete_flag";
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Autowired
    private CategoryBrandService categoryBrandService;

    @Autowired
    private CategoryParameterGroupService categoryParameterGroupService;

    @Autowired
    private CategorySpecificationService categorySpecificationService;

    @Override
    public List<Category> dbList(String parentId) {
        return this.list(new LambdaQueryWrapper<Category>().eq(Category::getParentId, parentId));
    }

    @Override
    public Category getCategoryById(String id) {
        return this.getById(id);
    }

    /**
     * 根据分类id集合获取所有分类根据层级排序
     *
     * @param ids 分类ID集合
     * @return 商品分类列表
     */
    @Override
    public List<Category> listByIdsOrderByLevel(List<String> ids) {
        return this.list(new LambdaQueryWrapper<Category>().in(Category::getId, ids).orderByAsc(Category::getLevel));
    }

    @Override
    public List<CategoryVO> categoryTree() {
        List<CategoryVO> categoryVOList = (List<CategoryVO>) cache.get(CachePrefix.CATEGORY.getPrefix());
        if (categoryVOList != null) {
            return categoryVOList;
        }

        //获取全部分类
        LambdaQueryWrapper<Category> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Category::getDeleteFlag, false);
        List<Category> list = this.list(queryWrapper);

        //构造分类树
        categoryVOList = new ArrayList<>();
        for (Category category : list) {
            if ("0".equals(category.getParentId())) {
                CategoryVO categoryVO = new CategoryVO(category);
                categoryVO.setChildren(findChildren(list, categoryVO));
                categoryVOList.add(categoryVO);
            }
        }
        categoryVOList.sort(Comparator.comparing(Category::getSortOrder));
        if (!categoryVOList.isEmpty()) {
            cache.put(CachePrefix.CATEGORY.getPrefix(), categoryVOList);
            cache.put(CachePrefix.CATEGORY_ARRAY.getPrefix(), list);
        }
        return categoryVOList;
    }

    @Override
    public List<CategoryVO> getStoreCategory(String[] categories) {
        List<String> arr = Arrays.asList(categories.clone());
        return categoryTree().stream()
                .filter(item -> arr.contains(item.getId())).collect(Collectors.toList());
    }

    @Override
    public List<Category> firstCategory() {
        QueryWrapper<Category> queryWrapper = Wrappers.query();
        queryWrapper.eq("level", 0);
        return list(queryWrapper);
    }

    @Override
    public List<CategoryVO> listAllChildren(String parentId) {
        if ("0".equals(parentId)) {
            return categoryTree();
        }
        //循环代码，找到对象，把他的子分类返回
        List<CategoryVO> topCatList = categoryTree();
        for (CategoryVO item : topCatList) {
            if (item.getId().equals(parentId)) {
                return item.getChildren();
            } else {
                return getChildren(parentId, item.getChildren());
            }
        }
        return new ArrayList<>();
    }

    @Override
    public List<CategoryVO> listAllChildren() {

        //获取全部分类
        List<Category> list = this.list();

        //构造分类树
        List<CategoryVO> categoryVOList = new ArrayList<>();
        for (Category category : list) {
            if (("0").equals(category.getParentId())) {
                CategoryVO categoryVO = new CategoryVO(category);
                categoryVO.setChildren(findChildren(list, categoryVO));
                categoryVOList.add(categoryVO);
            }
        }
        categoryVOList.sort(Comparator.comparing(Category::getSortOrder));
        return categoryVOList;
    }

    /**
     * 获取指定分类的分类名称
     *
     * @param ids 指定分类id集合
     * @return 分类名称集合
     */
    @Override
    public List<String> getCategoryNameByIds(List<String> ids) {
        List<String> categoryName = new ArrayList<>();
        List<Category> categoryVOList = (List<Category>) cache.get(CachePrefix.CATEGORY_ARRAY.getPrefix());
        //如果缓存中为空，则重新获取缓存
        if (categoryVOList == null) {
            categoryTree();
            categoryVOList = (List<Category>) cache.get(CachePrefix.CATEGORY_ARRAY.getPrefix());
        }
        //还为空的话，直接返回
        if (categoryVOList == null) {
            return new ArrayList<>();
        }
        //循环顶级分类
        for (Category category : categoryVOList) {
            //循环查询的id匹配
            for (String id : ids) {
                if (category.getId().equals(id)) {
                    //写入商品分类
                    categoryName.add(category.getName());
                }
            }
        }
        return categoryName;
    }

    @Override
    public List<Category> findByAllBySortOrder(Category category) {
        QueryWrapper<Category> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(category.getLevel() != null, "level", category.getLevel())
                .eq(CharSequenceUtil.isNotBlank(category.getName()), "name", category.getName())
                .eq(category.getParentId() != null, "parent_id", category.getParentId())
                .ne(category.getId() != null, "id", category.getId())
                .eq(DELETE_FLAG_COLUMN, false)
                .orderByAsc("sort_order");
        return this.baseMapper.selectList(queryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean saveCategory(Category category) {
        //判断分类佣金是否正确
        if (category.getCommissionRate() < 0) {
            throw new ServiceException(ResultCode.CATEGORY_COMMISSION_RATE_ERROR);
        }
        //子分类与父分类的状态一致
        if (category.getParentId() != null && !("0").equals(category.getParentId())) {
            Category parentCategory = this.getById(category.getParentId());
            category.setDeleteFlag(parentCategory.getDeleteFlag());
        }
        this.save(category);
        removeCache();
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateCategory(Category category) {
        //判断分类佣金是否正确
        if (category.getCommissionRate() < 0) {
            throw new ServiceException(ResultCode.CATEGORY_COMMISSION_RATE_ERROR);
        }
        //判断父分类与子分类的状态是否一致
        if (category.getParentId() != null && !"0".equals(category.getParentId())) {
            Category parentCategory = this.getById(category.getParentId());
            if (!parentCategory.getDeleteFlag().equals(category.getDeleteFlag())) {
                throw new ServiceException(ResultCode.CATEGORY_DELETE_FLAG_ERROR);
            }
        }
        UpdateWrapper<Category> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("id", category.getId())
                .set("name", category.getName())
                .set("image", category.getImage())
                .set("sort_order", category.getSortOrder())
                .set(DELETE_FLAG_COLUMN, category.getDeleteFlag())
                .set("commission_rate", category.getCommissionRate());
        this.baseMapper.update(category, updateWrapper);
        removeCache();
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(String id) {
        this.removeById(id);
        removeCache();
        //删除关联关系
        categoryBrandService.deleteByCategoryId(id);
        categoryParameterGroupService.deleteByCategoryId(id);
        categorySpecificationService.deleteByCategoryId(id);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateCategoryStatus(String categoryId, Boolean enableOperations) {
        //禁用子分类
        CategoryVO categoryVO = new CategoryVO(this.getById(categoryId));
        List<String> ids = new ArrayList<>();
        ids.add(categoryVO.getId());
        this.findAllChild(categoryVO);
        this.findAllChildIds(categoryVO, ids);
        LambdaUpdateWrapper<Category> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.in(Category::getId, ids);
        updateWrapper.set(Category::getDeleteFlag, enableOperations);
        this.update(updateWrapper);
        removeCache();
    }

    /**
     * 递归树形VO
     *
     * @param categories 分类列表
     * @param categoryVO 分类VO
     * @return 分类VO列表
     */
    private List<CategoryVO> findChildren(List<Category> categories, CategoryVO categoryVO) {
        List<CategoryVO> children = new ArrayList<>();
        categories.forEach(item -> {
            if (item.getParentId().equals(categoryVO.getId())) {
                CategoryVO temp = new CategoryVO(item);
                temp.setChildren(findChildren(categories, temp));
                children.add(temp);
            }
        });

        return children;
    }

    /**
     * 条件查询分类
     *
     * @param category 分类VO
     */
    private void findAllChild(CategoryVO category) {
        LambdaQueryWrapper<Category> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Category::getParentId, category.getId());
        List<Category> categories = this.list(queryWrapper);
        List<CategoryVO> categoryVOList = new ArrayList<>();
        for (Category category1 : categories) {
            categoryVOList.add(new CategoryVO(category1));
        }
        category.setChildren(categoryVOList);
        if (!categoryVOList.isEmpty()) {
            categoryVOList.forEach(this::findAllChild);
        }
    }

    /**
     * 获取所有的子分类ID
     *
     * @param category 分类
     * @param ids      ID列表
     */
    private void findAllChildIds(CategoryVO category, List<String> ids) {
        if (category.getChildren() != null && !category.getChildren().isEmpty()) {
            for (CategoryVO child : category.getChildren()) {
                ids.add(child.getId());
                this.findAllChildIds(child, ids);
            }
        }
    }

    /**
     * 递归自身，找到id等于parentId的对象，获取他的children 返回
     *
     * @param parentId       父ID
     * @param categoryVOList 分类VO
     * @return 子分类列表VO
     */
    private List<CategoryVO> getChildren(String parentId, List<CategoryVO> categoryVOList) {
        for (CategoryVO item : categoryVOList) {
            if (item.getId().equals(parentId)) {
                return item.getChildren();
            }
            if (item.getChildren() != null && !item.getChildren().isEmpty()) {
                return getChildren(parentId, categoryVOList);
            }
        }
        return categoryVOList;
    }

    /**
     * 清除缓存
     */
    private void removeCache() {
        cache.remove(CachePrefix.CATEGORY.getPrefix());
    }
}