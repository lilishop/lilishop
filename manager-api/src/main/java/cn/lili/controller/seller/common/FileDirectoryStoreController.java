package cn.lili.controller.seller.common;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.file.entity.FileDirectory;
import cn.lili.modules.file.entity.dto.FileDirectoryDTO;
import cn.lili.modules.file.service.FileDirectoryService;
import cn.lili.modules.file.service.FileService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 文件目录管理接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Tag(name = "文件目录管理接口")
@RequestMapping("/store/common/fileDirectory")
@RequiredArgsConstructor
public class FileDirectoryStoreController {

    private final FileDirectoryService fileDirectoryService;
    private final FileService fileService;

    @Operation(summary = "获取文件目录列表")
    @GetMapping
    public ResultMessage<List<FileDirectoryDTO>> getSceneFileList() {
        return ResultUtil.data(fileDirectoryService.getFileDirectoryList(UserContext.getCurrentUser().getId()));
    }

    @Operation(summary = "添加文件目录")
    @PostMapping
    public ResultMessage<FileDirectory> addSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        fileDirectory.setDirectoryType(UserContext.getCurrentUser().getRole().name());
        fileDirectory.setOwnerId(UserContext.getCurrentUser().getId());
        fileDirectoryService.save(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "修改文件目录")
    @PutMapping
    public ResultMessage<FileDirectory> editSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        fileDirectory.setDirectoryType(UserContext.getCurrentUser().getRole().name());
        fileDirectory.setOwnerId(UserContext.getCurrentUser().getId());
        fileDirectoryService.updateById(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "删除文件目录")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> deleteSceneFileList(@PathVariable String id) {
        if (fileService.countByDirectory(id)) {
            return ResultUtil.error(ResultCode.FILE_DIRECTORY_NOT_EMPTY);
        }
        fileDirectoryService.removeById(id);
        return ResultUtil.success();
    }

}
