package cn.lili.modules.page.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dos.ArticleCategory;
import cn.lili.modules.page.entity.enums.ArticleCategoryEnum;
import cn.lili.modules.page.entity.enums.ArticleEnum;
import cn.lili.modules.page.entity.vos.ArticleCategoryVO;
import cn.lili.modules.page.mapper.ArticleCategoryMapper;
import cn.lili.modules.page.service.ArticleCategoryService;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * 文章分类业务层实现
 *
 * @author pikachu
 * @since 2020-05-5 15:10:16
 */
@Service
public class ArticleCategoryServiceImpl extends ServiceImpl<ArticleCategoryMapper, ArticleCategory> implements ArticleCategoryService {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;
    /**
     * 顶级父分类ID
     */
    private String parentId = "0";
    /**
     * 最大分类等级
     */
    private int maxLevel = 2;

    @Override
    public ArticleCategory saveArticleCategory(ArticleCategory articleCategory) {
        //非顶级分类
        if (articleCategory.getParentId() != null && !parentId.equals(articleCategory.getParentId())) {
            ArticleCategory parent = this.getById(articleCategory.getParentId());
            if (parent == null) {
                throw new ServiceException(ResultCode.ARTICLE_CATEGORY_PARENT_NOT_EXIST);
            }
            if (articleCategory.getLevel() >= maxLevel) {
                throw new ServiceException(ResultCode.ARTICLE_CATEGORY_BEYOND_TWO);
            }
        }
        articleCategory.setType(ArticleCategoryEnum.OTHER.name());
        this.save(articleCategory);
        //清除文章分类缓存
        this.clearCache();
        return articleCategory;
    }


    @Override
    public ArticleCategory updateArticleCategory(ArticleCategory articleCategory) {
        //非顶级分类校验是否存在
        if (!parentId.equals(articleCategory.getParentId())) {
            ArticleCategory parent = this.getById(articleCategory.getParentId());
            if (parent == null) {
                throw new ServiceException(ResultCode.ARTICLE_CATEGORY_PARENT_NOT_EXIST);
            }
            //替换catPath 根据path规则来匹配级别
            if (articleCategory.getLevel() >= maxLevel) {
                throw new ServiceException(ResultCode.ARTICLE_CATEGORY_BEYOND_TWO);
            }
        }
        //验证分类名称是否重复
        ArticleCategory category = this.getOne(
                new LambdaQueryWrapper<ArticleCategory>().eq(ArticleCategory::getArticleCategoryName, articleCategory.getArticleCategoryName()));
        if (category != null && !category.getId().equals(articleCategory.getId())) {
            throw new ServiceException(ResultCode.ARTICLE_CATEGORY_NAME_EXIST);
        }
        if (this.updateById(articleCategory)) {
            //清除文章分类
            this.clearCache();
            return category;
        }
        return null;
    }

    @Override
    public boolean deleteById(String id) {

        LambdaQueryWrapper<ArticleCategory> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(ArticleCategory::getParentId, id);

        //查看文章分类下是否有分类
        if (this.count(lambdaQueryWrapper) > 0) {
            throw new ServiceException(ResultCode.ARTICLE_CATEGORY_DELETE_ERROR);
        }

        //查看文章分类下是否有文章
        LambdaQueryWrapper<Article> articleLambdaQueryWrapper = new LambdaQueryWrapper<>();
        articleLambdaQueryWrapper.eq(Article::getCategoryId, id);
        if (articleService.count(articleLambdaQueryWrapper) > 0) {
            throw new ServiceException(ResultCode.ARTICLE_CATEGORY_HAS_ARTICLE);
        }
        //判断是否为默认的分类
        if (!this.getById(id).getType().equals(ArticleEnum.OTHER.name())) {
            throw new ServiceException(ResultCode.ARTICLE_CATEGORY_NO_DELETION);
        }

        //清除文章分类缓存
        this.clearCache();
        //删除文章分类
        return this.removeById(id);
    }

    @Override
    public List<ArticleCategoryVO> allChildren() {
        //从缓存取所有的分类
        Object all = cache.get(CachePrefix.ARTICLE_CATEGORY.getPrefix());
        List<ArticleCategoryVO> articleCategories;
        if (all == null) {
            //调用初始化分类缓存方法
            articleCategories = initCategory();
        } else {
            articleCategories = (List<ArticleCategoryVO>) all;
        }
        return articleCategories;
    }

    /**
     * 初始化所有文章分类
     *
     * @return 文章分类集合
     */
    private List<ArticleCategoryVO> initCategory() {
        List<ArticleCategory> articleCategories = this.list();
        List<ArticleCategoryVO> tree = new ArrayList<>();
        articleCategories.forEach(item -> {
            if (item.getLevel() == 0) {
                ArticleCategoryVO articleCategoryVO = new ArticleCategoryVO(item);
                initChild(articleCategoryVO, articleCategories);
                tree.add(articleCategoryVO);
            }
        });
        //对一级菜单排序
        tree.sort(new Comparator<ArticleCategoryVO>() {
            @Override
            public int compare(ArticleCategoryVO o1, ArticleCategoryVO o2) {
                return o1.getSort().compareTo(o2.getSort());
            }
        });
        cache.put(CachePrefix.ARTICLE_CATEGORY.getPrefix(), tree);

        return tree;
    }

    /**
     * 递归初始化子树
     *
     * @param tree              树结构
     * @param articleCategories 数据库对象集合
     */
    private void initChild(ArticleCategoryVO tree, List<ArticleCategory> articleCategories) {
        if (articleCategories == null) {
            return;
        }
        articleCategories.stream()
                .filter(item -> (item.getParentId().equals(tree.getId())))
                .forEach(child -> {
                    ArticleCategoryVO childTree = new ArticleCategoryVO(child);
                    initChild(childTree, articleCategories);
                    tree.getChildren().add(childTree);
                });
    }

    /**
     * 清除缓存中的文章分类
     */
    private void clearCache() {
        cache.remove(CachePrefix.ARTICLE_CATEGORY.getPrefix());
    }


    @Autowired
    public void setArticleService(ArticleService articleService) {
        this.articleService = articleService;
    }
}