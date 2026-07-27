package cn.lili.modules.file.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.file.entity.FileDirectory;
import cn.lili.modules.file.entity.dto.FileDirectoryDTO;
import cn.lili.modules.file.mapper.FileDirectoryMapper;
import cn.lili.modules.file.service.FileDirectoryService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 文件管理业务层实现
 *
 * @author Chopper
 * @since 2020/11/26 17:50
 */
@Service
@RequiredArgsConstructor
public class FileDirectoryServiceImpl extends ServiceImpl<FileDirectoryMapper, FileDirectory> implements FileDirectoryService {


    @Override
    public void addFileDirectory(UserEnums userEnum, String id, String ownerName) {
        FileDirectory fileDirectory = new FileDirectory();
        fileDirectory.setOwnerId(id);
        fileDirectory.setDirectoryName(ownerName);
        fileDirectory.setDirectoryType(userEnum.name());
        this.save(fileDirectory);
    }

    @Override
    public List<FileDirectoryDTO> getFileDirectoryList(String ownerId) {
        return getFileDirectoryList(ownerId, null);
    }

    @Override
    public List<FileDirectoryDTO> getFileDirectoryList(String ownerId, String directoryType) {
        LambdaQueryWrapper<FileDirectory> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(FileDirectory::getOwnerId, ownerId);
        // 按 directoryType 过滤，避免商家与平台分组混用
        if (CharSequenceUtil.isNotEmpty(directoryType)) {
            queryWrapper.eq(FileDirectory::getDirectoryType, directoryType);
        }
        List<FileDirectory> fileDirectoryList = this.list(queryWrapper);
        List<FileDirectoryDTO> fileDirectoryDTOList = new ArrayList<>();

        fileDirectoryList.forEach(item -> {
            if (item.getLevel() == 0) {
                FileDirectoryDTO fileDirectoryDTO = new FileDirectoryDTO(item);
                initChild(fileDirectoryDTO, fileDirectoryList);
                fileDirectoryDTOList.add(fileDirectoryDTO);
            }
        });

        return fileDirectoryDTOList;
    }

    @Override
    public void assertDirectoryOwned(String directoryId, String ownerId, String directoryType) {
        if (CharSequenceUtil.isBlank(directoryId)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        FileDirectory directory = this.getById(directoryId);
        if (directory == null) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        // 同时校验 ownerId 与 directoryType，防止跨店铺或跨角色操作
        if (!CharSequenceUtil.equals(ownerId, directory.getOwnerId())
                || !CharSequenceUtil.equals(directoryType, directory.getDirectoryType())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
    }


    /**
     * 递归初始化子树
     */
    private void initChild(FileDirectoryDTO fileDirectoryDTO, List<FileDirectory> fileDirectoryList) {
        if (fileDirectoryList == null) {
            return;
        }
        fileDirectoryList.stream()
                .filter(item -> (item.getParentId().equals(fileDirectoryDTO.getId())))
                .forEach(child -> {
                    FileDirectoryDTO childTree = new FileDirectoryDTO(child);
                    initChild(childTree, fileDirectoryList);
                    fileDirectoryDTO.getChildren().add(childTree);
                });
    }
}
