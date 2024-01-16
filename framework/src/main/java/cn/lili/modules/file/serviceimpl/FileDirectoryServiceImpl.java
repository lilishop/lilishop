package cn.lili.modules.file.serviceimpl;

import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
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
    public List<FileDirectoryDTO> getFileDirectoryList(String scene) {
        List<FileDirectory> fileDirectoryList = this.list(new LambdaQueryWrapper<FileDirectory>().eq(FileDirectory::getOwnerId, scene));
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