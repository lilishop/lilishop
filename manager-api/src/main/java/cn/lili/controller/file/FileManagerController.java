package cn.lili.controller.file;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.service.FileService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,文件管理管理接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Api(tags = "管理端,文件管理接口")
@RequestMapping("/manager/common/file")
public class FileManagerController {

    @Autowired
    private FileService fileService;


    @ApiOperation(value = "管理端管理所有图片")
    @GetMapping
    @ApiImplicitParam(name = "title", value = "名称模糊匹配")
    public ResultMessage<IPage<File>> adminFiles(File file, SearchVO searchVO, PageVO pageVo) {

        return ResultUtil.data(fileService.customerPage(file, searchVO, pageVo));
    }


    @ApiOperation(value = "文件重命名")
    @PostMapping(value = "/rename")
    public ResultMessage<File> upload(String id, String newName) {
        File file = fileService.getById(id);
        file.setName(newName);
        fileService.updateById(file);
        return ResultUtil.data(file);
    }

    @ApiOperation(value = "文件删除")
    @DeleteMapping(value = "/delete/{ids}")
    public ResultMessage delete(@PathVariable List<String> ids) {
        fileService.batchDelete(ids);
        return ResultUtil.success();
    }

}
