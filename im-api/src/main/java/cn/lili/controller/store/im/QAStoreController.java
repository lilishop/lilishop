package cn.lili.controller.store.im;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.QA;
import cn.lili.modules.im.service.QAService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * 管理端,自定义分词接口
 *
 * @author paulG
 * @since 2020/10/16
 **/
@Slf4j
@RestController
@Api(tags = "管理端,自定义分词接口")
@RequestMapping("/im/store/qa")
public class QAStoreController {

    @Autowired
    private QAService qaService;

    @ApiOperation(value = "添加问答")
    @PostMapping
    public ResultMessage<QA> addCustomWords(@Valid QA qa) {
        qaService.save(qa);
        return ResultUtil.data(qa);
    }

    @ApiOperation(value = "修改自定义问答")
    @PutMapping
    public ResultMessage<QA> updateCustomWords(@Valid QA qa) {
        qaService.updateById(qa);
        return ResultUtil.data(qa);
    }

    @ApiOperation(value = "删除自定义分词")
    @DeleteMapping("/{id}")
    public ResultMessage<String> deleteCustomWords(@NotNull @PathVariable String id) {
        qaService.removeById(id);
        return ResultUtil.success();
    }

    @ApiOperation(value = "分页获取自定义分词")
    @ApiImplicitParam(name = "word", value = "问题", required = true, dataType = "String", paramType = "query")
    @GetMapping("/page")
    public ResultMessage<IPage<QA>> getCustomWords(@RequestParam String word, PageVO pageVo) {
        return ResultUtil.data(qaService.getStoreQA(word, pageVo));
    }

}
