package cn.lili.controller.other;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.vo.CustomWordsVO;
import cn.lili.modules.search.service.CustomWordsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * 管理端,自定义分词接口
 *
 * @author paulG
 * @date 2020/10/16
 **/
@RestController
@Api(tags = "管理端,自定义分词接口")
@RequestMapping("/manager/manager/custom-words")
public class CustomWordsManagerController {

    /**
     * 分词
     */
    @Autowired
    private CustomWordsService customWordsService;

    @ApiOperation(value = "添加自定义分词")
    @PostMapping
    public ResultMessage<CustomWordsVO> addCustomWords(@Valid CustomWordsVO customWords) {
        if (customWordsService.addCustomWords(customWords)) {
            return ResultUtil.data(customWords);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @ApiOperation(value = "修改自定义分词")
    @PutMapping
    public ResultMessage<CustomWordsVO> updateCustomWords(@Valid CustomWordsVO customWords) {
        if (customWordsService.updateCustomWords(customWords)) {
            return ResultUtil.data(customWords);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @ApiOperation(value = "删除自定义分词")
    @ApiImplicitParam(name = "id", value = "文章ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping("/{id}")
    public ResultMessage<String> deleteCustomWords(@NotNull @PathVariable String id) {
        if (customWordsService.deleteCustomWords(id)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @ApiOperation(value = "分页获取自定义分词")
    @ApiImplicitParam(name = "words", value = "分词", required = true, dataType = "String", paramType = "query")
    @GetMapping
    public ResultMessage<IPage<CustomWords>> getCustomWords(@RequestParam String words, PageVO pageVo) {
        return ResultUtil.data(customWordsService.getCustomWordsByPage(words, pageVo));
    }


}
