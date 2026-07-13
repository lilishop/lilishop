package cn.lili.controller.manager.other;

import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Feedback;
import cn.lili.modules.page.service.FeedbackService;
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
 * 管理端,意见反馈接口
 *
 * @author Bulbasaur
 * @since 2020-05-5 15:10:16
 */
@RestController
@Tag(name = "管理端,意见反馈接口")
@RequestMapping("/manager/other/feedback")
public class FeedbackManagerController {

    /**
     * 意见反馈
     */
    @Autowired
    private FeedbackService feedbackService;

    @Operation(summary = "查询意见反馈列表")
    @GetMapping()
    public ResultMessage<IPage<Feedback>> page(PageVO pageVO) {
        return ResultUtil.data(feedbackService.page(PageUtil.initPage(pageVO)));
    }

    @Operation(summary = "查看意见反馈")
    @GetMapping("/{id}")
    public ResultMessage<Feedback> getFeedback(
            @Parameter(description = "意见反馈ID", required = true) @PathVariable String id) {
        return ResultUtil.data(this.feedbackService.getById(id));
    }

}
