package cn.lili.controller.manager.common;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
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
 * 平台端文件目录（素材分组）管理接口。
 * <p>
 * 分组按管理员维度隔离：ownerId 为当前管理员 ID，directoryType 为 MANAGER。
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Tag(name = "文件目录管理接口")
@RequestMapping("/manager/common/fileDirectory")
@RequiredArgsConstructor
public class FileDirectoryManagerController {

    private final FileDirectoryService fileDirectoryService;
    private final FileService fileService;

    @Operation(summary = "获取文件目录列表")
    @GetMapping
    public ResultMessage<List<FileDirectoryDTO>> getSceneFileList() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(fileDirectoryService.getFileDirectoryList(
                authUser.getId(), UserEnums.MANAGER.name()));
    }

    @Operation(summary = "添加文件目录")
    @PostMapping
    public ResultMessage<FileDirectory> addSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        AuthUser authUser = UserContext.getCurrentUser();
        fileDirectory.setDirectoryType(UserEnums.MANAGER.name());
        fileDirectory.setOwnerId(authUser.getId());
        fileDirectoryService.save(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "修改文件目录")
    @PutMapping
    public ResultMessage<FileDirectory> editSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        AuthUser authUser = UserContext.getCurrentUser();
        // 仅允许修改本人创建的管理员分组
        fileDirectoryService.assertDirectoryOwned(fileDirectory.getId(), authUser.getId(), UserEnums.MANAGER.name());
        fileDirectory.setDirectoryType(UserEnums.MANAGER.name());
        fileDirectory.setOwnerId(authUser.getId());
        fileDirectoryService.updateById(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "删除文件目录")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> deleteSceneFileList(@PathVariable String id) {
        AuthUser authUser = UserContext.getCurrentUser();
        // 仅允许删除本人创建的管理员分组
        fileDirectoryService.assertDirectoryOwned(id, authUser.getId(), UserEnums.MANAGER.name());
        if (fileService.countByDirectory(id)) {
            return ResultUtil.error(ResultCode.FILE_DIRECTORY_NOT_EMPTY);
        }
        fileDirectoryService.removeById(id);
        return ResultUtil.success();
    }

}
