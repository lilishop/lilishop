package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.CategoryParameterGroup;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.entity.vos.GoodsParamsGroupVO;
import cn.lili.modules.goods.entity.vos.GoodsParamsVO;
import cn.lili.modules.goods.mapper.CategoryMapper;
import cn.lili.modules.goods.service.CategoryService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;


/**
 * 商品分类业务层实现
 *
 * @author pikachu
 * @date 2020-02-23 15:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class CategoryServiceImpl extends ServiceImpl<CategoryMapper, Category> implements CategoryService {

    private static final String DELETE_FLAG_COLUMN = "delete_flag";
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public List<Category> dbList(String parentId) {
        return this.list(new LambdaQueryWrapper<Category>().eq(Category::getParentId, parentId));
    }

    @Override
    public List<CategoryVO> categoryTree() {
        if (cache.hasKey(CachePrefix.CATEGORY.getPrefix() + "tree")) {
            return (List<CategoryVO>) cache.get(CachePrefix.CATEGORY.getPrefix() + "tree");
        }

        //获取全部分类
        LambdaQueryWrapper<Category> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Category::getDeleteFlag, false);
        List<Category> list = this.list(queryWrapper);

        //构造分类树
        List<CategoryVO> categoryVOList = new ArrayList<>();
        for (Category category : list) {
            if ("0".equals(category.getParentId())) {
                CategoryVO categoryVO = new CategoryVO(category);
                categoryVO.setChildren(findChildren(list, categoryVO));
                categoryVOList.add(categoryVO);
            }
        }
        categoryVOList.sort(new Comparator<CategoryVO>() {
            @Override
            public int compare(CategoryVO o1, CategoryVO o2) {
                return o1.getSortOrder().compareTo(o2.getSortOrder());
            }
        });
        if (categoryVOList.size() != 0) {
            cache.put(CachePrefix.CATEGORY.getPrefix() + "tree", categoryVOList);
        }
        return categoryVOList;
    }

    @Override
    public List<CategoryVO> getStoreCategory(String[] categories) {
        List<String> arr = Arrays.asList(categories.clone());
        List<CategoryVO> categoryVOS = categoryTree().stream()
                .filter(item -> arr.contains(item.getId())).collect(Collectors.toList());
        return categoryVOS;

    }

    @Override
    public List<Category> firstCategory() {
        QueryWrapper queryWrapper = Wrappers.query();
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
            }
            return getChildren(parentId, item.getChildren());
        }
        return new ArrayList<>();
    }

    @Override
    public List<CategoryVO> listAllChildrenDB() {

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
        categoryVOList.sort(new Comparator<CategoryVO>() {
            @Override
            public int compare(CategoryVO o1, CategoryVO o2) {
                return o1.getSortOrder().compareTo(o2.getSortOrder());
            }
        });
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
        LambdaQueryWrapper<Category> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(Category::getId, ids);
        return this.baseMapper.getNamesByIds(queryWrapper);
    }

    @Override
    public List<Category> findByAllBySortOrder(Category category) {
        QueryWrapper<Category> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(category.getLevel() != null, "level", category.getLevel())
                .eq(StrUtil.isNotBlank(category.getName()), "name", category.getName())
                .eq(category.getParentId() != null, "parent_id", category.getParentId())
                .ne(category.getId() != null, "id", category.getId())
                .eq(DELETE_FLAG_COLUMN, false)
                .orderByAsc("sort_order");
        return this.baseMapper.selectList(queryWrapper);
    }

    @Override
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
    public void delete(String id) {
        this.removeById(id);
        removeCache();
    }

    @Override
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
        List<CategoryVO> categoryVOS = new ArrayList<>();
        for (Category category1 : categories) {
            categoryVOS.add(new CategoryVO(category1));
        }
        category.setChildren(categoryVOS);
        if (!categoryVOS.isEmpty()) {
            categoryVOS.forEach(this::findAllChild);
        }
    }

    /**
     * 拼装返回值
     *
     * @param paramList 参数列表
     * @return 拼装后的返回值
     */
    private List<GoodsParamsGroupVO> convertParamList(List<CategoryParameterGroup> groupList, List<GoodsParamsVO> paramList) {
        Map<String, List<GoodsParamsVO>> map = new HashMap<>(16);
        for (GoodsParamsVO param : paramList) {
            if (map.get(param.getGroupId()) != null) {
                map.get(param.getGroupId()).add(param);
            } else {
                List<GoodsParamsVO> list = new ArrayList<>();
                list.add(param);
                map.put(param.getGroupId(), list);
            }
        }
        List<GoodsParamsGroupVO> resList = new ArrayList<>();
        for (CategoryParameterGroup group : groupList) {
            GoodsParamsGroupVO list = new GoodsParamsGroupVO();
            list.setGroupName(group.getGroupName());
            list.setGroupId(group.getId());
            list.setParams(map.get(group.getId()));
            resList.add(list);
        }
        return resList;
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
     * @param parentId    父ID
     * @param categoryVOS 分类VO
     * @return 子分类列表VO
     */
    private List<CategoryVO> getChildren(String parentId, List<CategoryVO> categoryVOS) {
        for (CategoryVO item : categoryVOS) {
            if (item.getId().equals(parentId)) {
                return item.getChildren();
            }
            if (item.getChildren() != null && item.getChildren().size() > 0) {
                return getChildren(parentId, categoryVOS);
            }
        }
        return null;
    }

    /**
     * 清除缓存
     */
    private void removeCache() {
        cache.remove(CachePrefix.CATEGORY.getPrefix() + "tree");
    }
}