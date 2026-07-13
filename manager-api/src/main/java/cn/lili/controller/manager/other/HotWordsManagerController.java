package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.dto.HotWordsDTO;
import cn.lili.modules.search.entity.dto.HotWordsSearchParams;
import cn.lili.modules.search.service.HotWordsHistoryService;
import cn.lili.modules.search.service.HotWordsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.NotNull;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * 管理端,热词管理
 *
 * @author Chopper
 * @since 2018-07-04 21:50:52
 */
@RestController
@Tag(name = "管理端,热词管理")
@RequestMapping("/manager/hotwords/hotwords")
public class HotWordsManagerController {

    @Autowired
    private HotWordsService hotWordsService;


    @Autowired
    private HotWordsHistoryService hotWordsHistoryService;

    @Operation(summary = "获取热词")
    @GetMapping
    public ResultMessage<Object> getHotWords() {
        return ResultUtil.data(hotWordsService.getHotWords(100));
    }

    @Operation(summary = "设置热词")
    @PostMapping
    public ResultMessage<Object> setHotWords(@Validated HotWordsDTO hotWords) {
        hotWordsService.setHotWords(hotWords);
        return ResultUtil.success();
    }

    @Operation(summary = "删除热词")
    @DeleteMapping
    public ResultMessage<Object> deleteWords(String words) {
        hotWordsService.deleteHotWords(words);
        return ResultUtil.success();
    }

    @Operation(summary = "历史热词")
    @GetMapping("/history")
    public ResultMessage<Object> deleteWords(HistorySearchParams historySearchParams) {
        List<HotWordsHistory> hotWordsHistoryList = hotWordsHistoryService.queryByDay(historySearchParams.getDate());
        Collections.sort(hotWordsHistoryList);
        return ResultUtil.data(hotWordsHistoryList);
    }

    @Operation(summary = "热词统计")
    @GetMapping("/statistics")
    public ResultMessage<Object> deleteWords(HotWordsSearchParams hotWordsSearchParams) {
        return ResultUtil.data(hotWordsHistoryService.statistics(hotWordsSearchParams));
    }
}

@Data
class HistorySearchParams {
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @NotNull(message = "查询日期不能为空")
    private Date date;

}
