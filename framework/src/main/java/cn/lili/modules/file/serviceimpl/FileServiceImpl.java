package cn.lili.modules.file.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.entity.dto.FileOwnerDTO;
import cn.lili.modules.file.mapper.FileMapper;
import cn.lili.modules.file.plugin.FilePluginFactory;
import cn.lili.modules.file.service.FileService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
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
public class FileServiceImpl extends ServiceImpl<FileMapper, File> implements FileService {

    @Autowired
    private FilePluginFactory filePluginFactory;

    @Override
    public void batchDelete(List<String> ids) {

        LambdaQueryWrapper<File> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(File::getId, ids);

        List<File> files = this.list(queryWrapper);
        List<String> keys = new ArrayList<>();
        files.forEach(item -> keys.add(item.getFileKey()));
        filePluginFactory.filePlugin().deleteFile(keys);
        this.remove(queryWrapper);
    }

    @Override
    public void batchDeleteByDirectory(String directoryId) {
        LambdaQueryWrapper<File> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(File::getFileDirectoryId, directoryId);

        List<File> files = this.list(queryWrapper);
        List<String> keys = new ArrayList<>();
        files.forEach(item -> keys.add(item.getFileKey()));
        filePluginFactory.filePlugin().deleteFile(keys);
        this.remove(queryWrapper);
    }

    @Override
    public void batchDelete(List<String> ids, AuthUser authUser) {
        LambdaQueryWrapper<File> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(File::getId, ids);

        queryWrapper.eq(File::getUserEnums, authUser.getRole().name());
        //操作图片属性判定
        switch (authUser.getRole()) {
            case MEMBER:
                queryWrapper.eq(File::getOwnerId, authUser.getId());
                break;
            case STORE:
                queryWrapper.eq(File::getOwnerId, authUser.getStoreId());
                break;
            case MANAGER:
                break;
            default:
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        List<File> files = this.list(queryWrapper);
        List<String> keys = new ArrayList<>();
        files.forEach(item -> keys.add(item.getFileKey()));
        filePluginFactory.filePlugin().deleteFile(keys);
        this.remove(queryWrapper);
    }

    @Override
    public IPage<File> customerPage(FileOwnerDTO fileOwnerDTO) {
        LambdaQueryWrapper<File> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getName()), File::getName, fileOwnerDTO.getName())
                .eq(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileDirectoryId()),File::getFileDirectoryId, fileOwnerDTO.getFileDirectoryId())
                .like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileKey()), File::getFileKey, fileOwnerDTO.getFileKey())
                .like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileType()), File::getFileType, fileOwnerDTO.getFileType())
                .between(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getStartDate()) && CharSequenceUtil.isNotEmpty(fileOwnerDTO.getEndDate()),
                        File::getCreateTime, fileOwnerDTO.getStartDate(), fileOwnerDTO.getEndDate());
        return this.page(PageUtil.initPage(fileOwnerDTO), queryWrapper);
    }

    @Override
    public IPage<File> customerPageOwner(FileOwnerDTO fileOwnerDTO) {
        LambdaQueryWrapper<File> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getOwnerId()), File::getOwnerId, fileOwnerDTO.getOwnerId())
                .eq(File::getUserEnums, fileOwnerDTO.getUserEnums())
                .eq(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileDirectoryId()),File::getFileDirectoryId, fileOwnerDTO.getFileDirectoryId())
                .like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getName()), File::getName, fileOwnerDTO.getName())
                .like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileKey()), File::getFileKey, fileOwnerDTO.getFileKey())
                .like(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getFileType()), File::getFileType, fileOwnerDTO.getFileType())
                .between(CharSequenceUtil.isNotEmpty(fileOwnerDTO.getStartDate()) && CharSequenceUtil.isNotEmpty(fileOwnerDTO.getEndDate()),
                        File::getCreateTime, fileOwnerDTO.getStartDate(), fileOwnerDTO.getEndDate());
        return this.page(PageUtil.initPage(fileOwnerDTO), queryWrapper);
    }
}