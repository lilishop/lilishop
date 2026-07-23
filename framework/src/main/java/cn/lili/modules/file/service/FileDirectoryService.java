package cn.lili.modules.file.service;

import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.file.entity.FileDirectory;
import cn.lili.modules.file.entity.dto.FileDirectoryDTO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 文件管理业务层
 *
 * @author Chopper
 */
public interface FileDirectoryService extends IService<FileDirectory> {

    /**
     * 添加目录
     *
     * @param userEnum
     * @param id
     * @param ownerName
     */
    void addFileDirectory(UserEnums userEnum, String id, String ownerName);

    /**
     * 获取文件目录
     *
     * @param ownerId 拥有者
     * @return
     */
    List<FileDirectoryDTO> getFileDirectoryList(String ownerId);

    /**
     * 按拥有者与目录类型获取文件目录。
     * <p>
     * 商家端 ownerId 传 storeId、directoryType 传 STORE；
     * 平台端 ownerId 传管理员 ID、directoryType 传 MANAGER。
     *
     * @param ownerId       拥有者 ID（商家为 storeId，平台为管理员 ID）
     * @param directoryType 目录类型，对应 {@link UserEnums#name()}
     * @return 目录树
     */
    List<FileDirectoryDTO> getFileDirectoryList(String ownerId, String directoryType);

    /**
     * 校验目录归属，ownerId 或 directoryType 不匹配时抛出权限异常。
     * 用于修改、删除分组及商家上传前校验。
     *
     * @param directoryId   目录 ID
     * @param ownerId       期望的拥有者 ID
     * @param directoryType 期望的目录类型
     */
    void assertDirectoryOwned(String directoryId, String ownerId, String directoryType);
}