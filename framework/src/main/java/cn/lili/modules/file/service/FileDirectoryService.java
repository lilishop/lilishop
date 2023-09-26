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
}