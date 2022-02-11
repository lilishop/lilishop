package cn.lili.controller.common;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.Base64DecodeMultipartFile;
import cn.lili.common.utils.CommonUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.plugin.FileManagerPlugin;
import cn.lili.modules.file.service.FileService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.util.Objects;

/**
 * 文件上传接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@Slf4j
@RestController
@Api(tags = "文件上传接口")
@RequestMapping("/common/common/upload")
public class UploadController {

    @Autowired
    private FileService fileService;
    @Autowired
    private SettingService settingService;
    @Autowired
    private FileManagerPlugin fileManagerPlugin;
    @Autowired
    private Cache cache;

    @ApiOperation(value = "文件上传")
    @PostMapping(value = "/file")
    public ResultMessage<Object> upload(MultipartFile file,
                                        String base64,
                                        @RequestHeader String accessToken) {


        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        //如果用户未登录，则无法上传图片
        if (authUser == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        Setting setting = settingService.get(SettingEnum.OSS_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            throw new ServiceException(ResultCode.OSS_NOT_EXIST);
        }
        if (file == null || CharSequenceUtil.isEmpty(file.getContentType())) {
            throw new ServiceException(ResultCode.IMAGE_FILE_EXT_ERROR);
        }


        if (!CharSequenceUtil.containsAny(file.getContentType().toLowerCase(), "image")) {
            throw new ServiceException(ResultCode.FILE_TYPE_NOT_SUPPORT);
        }

        if (CharSequenceUtil.isNotBlank(base64)) {
            //base64上传
            file = Base64DecodeMultipartFile.base64Convert(base64);
        }
        String result;
        String fileKey = CommonUtil.rename(Objects.requireNonNull(file.getOriginalFilename()));
        File newFile = new File();
        try {
            InputStream inputStream = file.getInputStream();
            //上传至第三方云服务或服务器
            result = fileManagerPlugin.inputStreamUpload(inputStream, fileKey);
            //保存数据信息至数据库
            newFile.setName(file.getOriginalFilename());
            newFile.setFileSize(file.getSize());
            newFile.setFileType(file.getContentType());
            newFile.setFileKey(fileKey);
            newFile.setUrl(result);
            newFile.setCreateBy(authUser.getUsername());
            newFile.setUserEnums(authUser.getRole().name());
            //如果是店铺，则记录店铺id
            if (authUser.getRole().equals(UserEnums.STORE)) {
                newFile.setOwnerId(authUser.getStoreId());
            } else {
                newFile.setOwnerId(authUser.getId());
            }
            fileService.save(newFile);
        } catch (Exception e) {
            log.error("文件上传失败", e);
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        }
        return ResultUtil.data(result);
    }
}
