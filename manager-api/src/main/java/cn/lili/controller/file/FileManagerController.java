package cn.lili.controller.file;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.entity.dto.FileOwnerDTO;
import cn.lili.modules.file.service.FileService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "管理端,文件管理接口")
@RequestMapping("/manager/common/file")
public class FileManagerController {

    @Autowired
    private FileService fileService;


    @Operation(summary = "管理端管理所有文件")
    @GetMapping
    public ResultMessage<IPage<File>> adminFiles(
            @Parameter(description = "名称模糊匹配") FileOwnerDTO fileOwnerDTO) {

        return ResultUtil.data(fileService.customerPage(fileOwnerDTO));
    }


    @Operation(summary = "文件重命名")
    @PostMapping("/rename")
    public ResultMessage<File> upload(
            @Parameter(description = "文件ID", required = true) String id, 
            @Parameter(description = "新文件名", required = true) String newName) {
        File file = fileService.getById(id);
        file.setName(newName);
        fileService.updateById(file);
        return ResultUtil.data(file);
    }

    @Operation(summary = "文件删除")
    @DeleteMapping("/delete/{ids}")
    public ResultMessage delete(
            @Parameter(description = "文件ID列表", required = true) @PathVariable List<String> ids) {
        fileService.batchDelete(ids);
        return ResultUtil.success();
    }

}
