package cn.lili.controller.seller.common;

import cn.lili.cache.Cache;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.ResponseUtil;
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
 * 文件管理接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Tag(name = "文件管理接口")
@RequestMapping("/store/common/file")
public class FileStoreController {

    @Autowired
    private FileService fileService;

    @Autowired
    private Cache cache;

    @Operation(summary = "获取自己的图片资源")
    @GetMapping
    @Parameter(name = "title", description = "名称模糊匹配")
    public ResultMessage<IPage<File>> getFileList(@RequestHeader String accessToken, FileOwnerDTO fileOwnerDTO) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        if (authUser == null) {
            ResponseUtil.output(ThreadContextHolder.getHttpResponse(), 403, ResponseUtil.resultMap(false,
                    403, "登录已失效，请重新登录"));
            return null;
        }
        if (authUser.getRole().equals(UserEnums.MEMBER)) {
            fileOwnerDTO.setOwnerId(authUser.getId());
        } else if (authUser.getRole().equals(UserEnums.STORE)) {
            fileOwnerDTO.setOwnerId(authUser.getStoreId());
        }
        fileOwnerDTO.setUserEnums(authUser.getRole().name());
        return ResultUtil.data(fileService.customerPageOwner(fileOwnerDTO));
    }

    @Operation(summary = "文件重命名")
    @PostMapping("/rename")
    public ResultMessage<File> upload(@RequestHeader String accessToken, String id, String newName) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        File file = fileService.getById(id);
        if (file == null) {
            // 文件不存在时直接返回，避免后续 NPE
            throw new ServiceException(ResultCode.FILE_NOT_EXIST_ERROR);
        }
        file.setName(newName);
        switch (authUser.getRole()) {
            case MEMBER:
                if (file.getOwnerId().equals(authUser.getId()) && file.getUserEnums().equals(authUser.getRole().name())) {
                    break;
                }
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            case STORE:
                if (file.getOwnerId().equals(authUser.getStoreId()) && file.getUserEnums().equals(authUser.getRole().name())) {
                    break;
                }
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            case MANAGER:
                break;
            default:
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        fileService.updateById(file);
        return ResultUtil.data(file);
    }

    @Operation(summary = "文件删除")
    @DeleteMapping("/delete/{ids}")
    public ResultMessage delete(@RequestHeader String accessToken, @PathVariable List<String> ids) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        fileService.batchDelete(ids, authUser);
        return ResultUtil.success();
    }

}
