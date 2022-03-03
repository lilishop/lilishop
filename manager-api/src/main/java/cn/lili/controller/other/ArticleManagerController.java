package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.enums.ArticleEnum;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,文章接口
 *
 * @author pikachu
 * @since 2020-05-06 15:18:56
 */
@RestController
@Api(tags = "管理端,文章接口")
@RequestMapping("/manager/other/article")
public class ArticleManagerController {

    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;

    @ApiOperation(value = "查看文章")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<Article> get(@PathVariable String id) {

        return ResultUtil.data(articleService.getById(id));
    }

    @ApiOperation(value = "分页获取")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "categoryId", value = "文章分类ID", paramType = "query"),
            @ApiImplicitParam(name = "title", value = "标题", paramType = "query")
    })
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<ArticleVO>> getByPage(ArticleSearchParams articleSearchParams) {
        return ResultUtil.data(articleService.managerArticlePage(articleSearchParams));
    }

    @ApiOperation(value = "添加文章")
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Article> save(@RequestBody Article article) {
        article.setType(ArticleEnum.OTHER.name());
        articleService.save(article);
        return ResultUtil.data(article);
    }

    @ApiOperation(value = "修改文章")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, paramType = "path")
    @PutMapping(value = "update/{id}", consumes = "application/json", produces = "application/json")
    public ResultMessage<Article> update(@RequestBody Article article, @PathVariable("id") String id) {
        article.setId(id);
        return ResultUtil.data(articleService.updateArticle(article));
    }

    @ApiOperation(value = "修改文章状态")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "文章ID", required = true, paramType = "path"),
            @ApiImplicitParam(name = "status", value = "操作状态", required = true, paramType = "query")
    })
    @PutMapping("update/status/{id}")
    public ResultMessage<Article> updateStatus(@PathVariable("id") String id, boolean status) {
        articleService.updateArticleStatus(id, status);
        return ResultUtil.success();
    }


    @ApiOperation(value = "批量删除")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping(value = "/delByIds/{id}")
    public ResultMessage<Object> delAllByIds(@PathVariable String id) {
        articleService.customRemove(id);
        return ResultUtil.success();
    }


}
