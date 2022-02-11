package cn.lili.controller.common;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.entity.dto.FileOwnerDTO;
import cn.lili.modules.file.service.FileService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 文件管理管理接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Api(tags = "文件管理管理接口")
@RequestMapping("/common/file")
public class FileController {

    @Autowired
    private FileService fileService;

    @Autowired
    private Cache cache;

    @ApiOperation(value = "获取自己的图片资源")
    @GetMapping
    @ApiImplicitParam(name = "title", value = "名称模糊匹配")
    public ResultMessage<IPage<File>> getFileList(@RequestHeader String accessToken, File file, SearchVO searchVO, PageVO pageVo) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        FileOwnerDTO fileOwnerDTO = new FileOwnerDTO();
        //只有买家才写入自己id
        if (authUser.getRole().equals(UserEnums.MEMBER)) {
            fileOwnerDTO.setOwnerId(authUser.getId());
        }//如果是店铺，则写入店铺id
        else if (authUser.getRole().equals(UserEnums.STORE)) {
            fileOwnerDTO.setOwnerId(authUser.getStoreId());
        }
        fileOwnerDTO.setUserEnums(authUser.getRole().name());
        return ResultUtil.data(fileService.customerPageOwner(fileOwnerDTO, file, searchVO, pageVo));
    }

    @ApiOperation(value = "文件重命名")
    @PostMapping(value = "/rename")
    public ResultMessage<File> upload(@RequestHeader String accessToken, String id, String newName) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        File file = fileService.getById(id);
        file.setName(newName);
        //操作图片属性判定
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

    @ApiOperation(value = "文件删除")
    @DeleteMapping(value = "/delete/{ids}")
    public ResultMessage delete(@RequestHeader String accessToken, @PathVariable List<String> ids) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        fileService.batchDelete(ids, authUser);
        return ResultUtil.success();
    }

}
