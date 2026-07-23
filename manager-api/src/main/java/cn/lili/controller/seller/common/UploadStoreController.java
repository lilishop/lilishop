package cn.lili.controller.seller.common;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
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
import cn.lili.modules.file.entity.enums.FileCategoryEnum;
import cn.lili.modules.file.plugin.FilePluginFactory;
import cn.lili.modules.file.service.FileDirectoryService;
import cn.lili.modules.file.service.FileService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
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
@Tag(name = "文件上传接口")
@RequestMapping("/store/common/upload")
public class UploadStoreController {

    @Autowired
    private FileService fileService;
    @Autowired
    private FileDirectoryService fileDirectoryService;
    @Autowired
    private FilePluginFactory filePluginFactory;
    @Autowired
    private Cache cache;

    @Operation(summary = "文件上传")
    @PostMapping("/file")
    public ResultMessage<Object> upload(MultipartFile file,
                                        String base64,
                                        @RequestHeader String accessToken, @RequestParam String directoryPath) {

        directoryPath = normalizeDirectoryPath(directoryPath);

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        if (authUser == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        // 上传前校验目标分组属于当前店铺，防止跨店绑定 fileDirectoryId
        validateDirectoryPath(authUser, directoryPath);
        if (file == null) {
            throw new ServiceException(ResultCode.FILE_NOT_EXIST_ERROR);
        }
        if (CharSequenceUtil.isEmpty(file.getContentType())) {
            throw new ServiceException(ResultCode.IMAGE_FILE_EXT_ERROR);
        }

        if (!CharSequenceUtil.containsAny(Objects.requireNonNull(file.getContentType()).toLowerCase(), "image", "video", "pdf")) {
            throw new ServiceException(ResultCode.FILE_TYPE_NOT_SUPPORT);
        }

        if (CharSequenceUtil.isNotBlank(base64)) {
            file = Base64DecodeMultipartFile.base64Convert(base64);
        }
        String result;
        String fileKey = CommonUtil.rename(Objects.requireNonNull(file.getOriginalFilename()));
        File newFile = new File();
        try {
            InputStream inputStream = file.getInputStream();
            String scene = this.buildUploadScene(authUser);
            fileKey = scene + "/" + directoryPath + "/" + fileKey;
            result = filePluginFactory.filePlugin().inputStreamUpload(inputStream, fileKey, file.getContentType());
            newFile.setName(file.getOriginalFilename());
            newFile.setFileSize(file.getSize());
            newFile.setFileType(file.getContentType());
            newFile.setFileCategory(FileCategoryEnum.fromContentType(file.getContentType()).name());
            newFile.setFileKey(fileKey);
            newFile.setUrl(result);
            newFile.setCreateBy(authUser.getUsername());
            newFile.setUserEnums(authUser.getRole().name());
            if (authUser.getRole().equals(UserEnums.STORE)) {
                newFile.setOwnerId(authUser.getStoreId());
                newFile.setOwnerName(authUser.getStoreName());
            } else {
                newFile.setOwnerId(authUser.getId());
                newFile.setOwnerName(authUser.getNickName());
            }

            if (StrUtil.isNotEmpty(directoryPath)) {
                if (directoryPath.indexOf("/") > 0) {
                    newFile.setFileDirectoryId(directoryPath.substring(directoryPath.lastIndexOf("/") + 1));
                } else {
                    newFile.setFileDirectoryId(directoryPath);
                }
            }
            fileService.save(newFile);
        } catch (Exception e) {
            log.error("文件上传失败", e);
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR);
        }
        return ResultUtil.data(result);
    }

    private String buildUploadScene(AuthUser authUser) {
        String scene = authUser.getRole().name();
        if (UserEnums.STORE.equals(authUser.getRole()) && StrUtil.isNotBlank(authUser.getStoreId())) {
            return scene + "/" + authUser.getStoreId();
        }
        if (StrUtil.equalsAny(scene, UserEnums.MEMBER.name(), UserEnums.SEAT.name())) {
            return scene + "/" + authUser.getId();
        }
        return scene;
    }

    /**
     * 规范化上传分组路径：空值、undefined、null 统一归入 default 默认分组。
     */
    private String normalizeDirectoryPath(String directoryPath) {
        directoryPath = StrUtil.trimToEmpty(directoryPath);
        if (StrUtil.isBlank(directoryPath)
                || "undefined".equalsIgnoreCase(directoryPath)
                || "null".equalsIgnoreCase(directoryPath)) {
            return "default";
        }
        return directoryPath;
    }

    /**
     * 校验上传所选分组归属。
     * default 为系统默认目录，无需校验；商家上传时分组 ownerId 必须与 storeId 一致。
     */
    private void validateDirectoryPath(AuthUser authUser, String directoryPath) {
        if ("default".equals(directoryPath)) {
            return;
        }
        String directoryId = directoryPath;
        if (directoryPath.indexOf("/") > 0) {
            directoryId = directoryPath.substring(directoryPath.lastIndexOf("/") + 1);
        }
        if (UserEnums.STORE.equals(authUser.getRole())) {
            fileDirectoryService.assertDirectoryOwned(directoryId, authUser.getStoreId(), UserEnums.STORE.name());
        }
    }
}
