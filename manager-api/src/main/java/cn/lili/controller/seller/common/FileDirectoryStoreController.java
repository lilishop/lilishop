package cn.lili.controller.seller.common;

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
 * 商家端文件目录（素材分组）管理接口。
 * <p>
 * 分组按店铺维度隔离：ownerId 使用 storeId，而非店员 memberId，
 * 保证同店不同店员共享同一套素材分组。
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
        AuthUser authUser = UserContext.getCurrentUser();
        // 仅返回当前店铺、类型为 STORE 的分组
        return ResultUtil.data(fileDirectoryService.getFileDirectoryList(
                resolveDirectoryOwnerId(authUser), UserEnums.STORE.name()));
    }

    @Operation(summary = "添加文件目录")
    @PostMapping
    public ResultMessage<FileDirectory> addSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        AuthUser authUser = UserContext.getCurrentUser();
        // 强制写入店铺归属，防止前端传入其他 ownerId
        fileDirectory.setDirectoryType(UserEnums.STORE.name());
        fileDirectory.setOwnerId(resolveDirectoryOwnerId(authUser));
        fileDirectoryService.save(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "修改文件目录")
    @PutMapping
    public ResultMessage<FileDirectory> editSceneFileList(@RequestBody @Valid FileDirectory fileDirectory) {
        AuthUser authUser = UserContext.getCurrentUser();
        String ownerId = resolveDirectoryOwnerId(authUser);
        // 修改前校验分组属于当前店铺
        fileDirectoryService.assertDirectoryOwned(fileDirectory.getId(), ownerId, UserEnums.STORE.name());
        fileDirectory.setDirectoryType(UserEnums.STORE.name());
        fileDirectory.setOwnerId(ownerId);
        fileDirectoryService.updateById(fileDirectory);
        return ResultUtil.data(fileDirectory);
    }

    @Operation(summary = "删除文件目录")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> deleteSceneFileList(@PathVariable String id) {
        AuthUser authUser = UserContext.getCurrentUser();
        fileDirectoryService.assertDirectoryOwned(id, resolveDirectoryOwnerId(authUser), UserEnums.STORE.name());
        if (fileService.countByDirectory(id)) {
            return ResultUtil.error(ResultCode.FILE_DIRECTORY_NOT_EMPTY);
        }
        fileDirectoryService.removeById(id);
        return ResultUtil.success();
    }

    /**
     * 解析分组归属 ID。
     * 商家角色使用 storeId，与素材文件 ownerId 保持一致；其他角色回退为当前用户 ID。
     */
    private String resolveDirectoryOwnerId(AuthUser authUser) {
        if (UserEnums.STORE.equals(authUser.getRole())) {
            return authUser.getStoreId();
        }
        return authUser.getId();
    }

}
