package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.enums.ArticleEnum;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,文章接口
 *
 * @author pikachu
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "管理端,文章接口")
@RequestMapping("/manager/other/article")
public class ArticleManagerController {

    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;

    @Operation(summary = "查看文章")
    @Parameter(name = "id", description = "文章ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Article> get(
            @PathVariable String id) {

        return ResultUtil.data(articleService.getById(id));
    }

    @Operation(summary = "根据类型查看文章")
    @Parameter(name = "type", description = "文章类型", required = true)
    @GetMapping("/type/{type}")
    public ResultMessage<Article> getByType(
            @PathVariable String type) {

        return ResultUtil.data(articleService.customGetByType(type));
    }

    @Operation(summary = "分页获取")
    @Parameter(name = "articleSearchParams", description = "查询参数", required = true)
    @GetMapping("/getByPage")
    public ResultMessage<IPage<ArticleVO>> getByPage(ArticleSearchParams articleSearchParams) {
        return ResultUtil.data(articleService.managerArticlePage(articleSearchParams));
    }

    @Operation(summary = "添加文章")
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Article> save(@RequestBody Article article) {
        article.setType(ArticleEnum.OTHER.name());
        articleService.save(article);
        return ResultUtil.data(article);
    }

    @Operation(summary = "修改文章--文章id")
    @Parameter(name = "article", description = "文章信息", required = true)
    @Parameter(name = "id", description = "文章ID", required = true)
    @PutMapping(value="/update/{id}", consumes = "application/json", produces = "application/json")
    public ResultMessage<Article> update(@RequestBody Article article, 
            @PathVariable("id") String id) {
        article.setId(id);
        return ResultUtil.data(articleService.updateArticle(article));
    }

    @Operation(summary = "修改文章--文章类型")
    @Parameter(name = "article", description = "文章信息", required = true)
    @Parameter(name = "type", description = "文章类型", required = true)
    @Parameter(name = "id", description = "文章ID", required = true)
    @PutMapping(value = "/updateArticle/{type}/{id}", consumes = "application/json", produces = "application/json")
    public ResultMessage<Article> updateArticle(@RequestBody Article article, 
            @PathVariable("type") String type, @PathVariable("id") String id) {
        article.setId(id);
        article.setType(type);
        return ResultUtil.data(articleService.updateArticleType(article));
    }

    @Operation(summary = "修改文章状态")
    @Parameter(name = "id", description = "文章ID", required = true)
    @Parameter(name = "status", description = "操作状态", required = true)
    @PutMapping("update/status/{id}")
    public ResultMessage<Article> updateStatus(
            @PathVariable("id") String id,
            @RequestParam boolean status) {
        articleService.updateArticleStatus(id, status);
        return ResultUtil.success();
    }


    @Operation(summary = "批量删除")
    @DeleteMapping("/delByIds/{id}")
    public ResultMessage<Object> delAllByIds(
            @Parameter(description = "文章ID", required = true) @PathVariable String id) {
        articleService.customRemove(id);
        return ResultUtil.success();
    }


}
