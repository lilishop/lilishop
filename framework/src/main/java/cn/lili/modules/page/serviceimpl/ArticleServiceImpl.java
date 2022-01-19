package cn.lili.modules.page.serviceimpl;


import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.BeanUtil;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.enums.ArticleEnum;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.mapper.ArticleMapper;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 文章业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 11:40 上午
 */
@Service
public class ArticleServiceImpl extends ServiceImpl<ArticleMapper, Article> implements ArticleService {

    @Override
    public IPage<ArticleVO> managerArticlePage(ArticleSearchParams articleSearchParams) {
        articleSearchParams.setSort("a.sort");
        return this.baseMapper.getArticleList(PageUtil.initPage(articleSearchParams), articleSearchParams.queryWrapper());
    }

    @Override
    public IPage<ArticleVO> articlePage(ArticleSearchParams articleSearchParams) {
        articleSearchParams.setSort("a.sort");
        QueryWrapper queryWrapper = articleSearchParams.queryWrapper();
        queryWrapper.eq("open_status",true);
        return this.baseMapper.getArticleList(PageUtil.initPage(articleSearchParams), queryWrapper);
    }

    @Override
    public List<Article> list(String categoryId) {

        QueryWrapper<Article> queryWrapper = Wrappers.query();
        queryWrapper.eq(StringUtils.isNotBlank(categoryId), "category_id", categoryId);
        return this.list(queryWrapper);
    }


    @Override
    public Article updateArticle(Article article) {
        Article oldArticle = this.getById(article.getId());
        BeanUtil.copyProperties(article, oldArticle);
        this.updateById(oldArticle);
        return oldArticle;
    }

    @Override
    public void customRemove(String id) {
        //判断是否为默认文章
        if(this.getById(id).getType().equals(ArticleEnum.OTHER.name())){
            this.removeById(id);
        }else{
            throw new ServiceException(ResultCode.ARTICLE_NO_DELETION);
        }
    }

    @Override
    public Article customGet(String id) {
        return this.getById(id);
    }

    @Override
    public Article customGetByType(String type) {
        if(!StrUtil.equals(type, ArticleEnum.OTHER.name())){
            return this.getOne(new LambdaUpdateWrapper<Article>().eq(Article::getType,type));
        }
        return null;
    }

    @Override
    public Boolean updateArticleStatus(String id, boolean status) {
        Article article=this.getById(id);
        article.setOpenStatus(status);
        return this.updateById(article);
    }
}