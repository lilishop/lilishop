package cn.lili.controller.manager.wxchannels;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wxchannels.entity.dos.WxChannelCategory;
import cn.lili.modules.wxchannels.entity.dto.WxChannelCategorySubmitDTO;
import cn.lili.modules.wxchannels.entity.dto.WxChannelsCategoryDTO;
import cn.lili.modules.wxchannels.service.WxChannelCategoryService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@Tag(name = "管理端,微信视频号类目接口")
@RequestMapping("/manager/wxchannels/category")
public class WxChannelCategoryManagerController {

    @Autowired
    private WxChannelCategoryService wxChannelCategoryService;

    @GetMapping
    @Operation(summary = "分页获取类目申请")
    @Parameter(name = "status", description = "状态 APPROVED,PENDING,REJECTED")
    public ResultMessage<IPage<WxChannelCategory>> page(PageVO page, String status) {
        return ResultUtil.data(wxChannelCategoryService.page(page, status));
    }

    @PutMapping("/{id}")
    @Operation(summary = "编辑类目映射")
    public ResultMessage<WxChannelCategory> update(@PathVariable String id, @RequestBody WxChannelCategory body) {
        body.setId(id);
        wxChannelCategoryService.updateById(body);
        return ResultUtil.data(body);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "删除类目申请")
    public ResultMessage<Object> delete(@PathVariable String id) {
        wxChannelCategoryService.removeById(id);
        return ResultUtil.success();
    }

    @PostMapping("/submit")
    @Operation(summary = "批量提审类目")
    public ResultMessage<Object> submit(@Valid @RequestBody WxChannelCategorySubmitDTO dto) {
        wxChannelCategoryService.submit(dto);
        return ResultUtil.success();
    }

    @GetMapping("/third")
    @Operation(summary = "获取微信小程序三级类目（缓存一天）")
    @Parameter(name = "forceRefresh", description = "是否强制刷新缓存，默认false")
    public ResultMessage<List<WxChannelsCategoryDTO>> third(@RequestParam(value = "forceRefresh", required = false, defaultValue = "false") Boolean forceRefresh) {
        return ResultUtil.data(wxChannelCategoryService.listAllThirdCategory(Boolean.TRUE.equals(forceRefresh)));
    }
}
