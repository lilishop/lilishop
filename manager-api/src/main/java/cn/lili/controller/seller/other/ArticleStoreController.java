package cn.lili.controller.seller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Article;
import cn.lili.modules.page.entity.dto.ArticleSearchParams;
import cn.lili.modules.page.entity.vos.ArticleVO;
import cn.lili.modules.page.service.ArticleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 店铺端,文章接口
 *
 * @author pikachu
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "店铺端,文章接口")
@RequestMapping("/store/other/article")
public class ArticleStoreController {

    /**
     * 文章
     */
    @Autowired
    private ArticleService articleService;

    @Operation(description = "分页获取")
    @Parameter(name = "articleSearchParams", description = "文章查询参数")
    @Parameter(name = "pageVO", description = "分页参数")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<ArticleVO>> getByPage(ArticleSearchParams articleSearchParams) {
        return ResultUtil.data(articleService.articlePage(articleSearchParams));
    }

    @Operation(description = "查看文章")
    @Parameter(name = "id", description = "文章ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Article> get(@PathVariable String id) {

        return ResultUtil.data(articleService.getById(id));
    }
}
