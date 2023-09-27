package cn.lili.modules.file.service;

import cn.lili.common.security.AuthUser;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.file.entity.File;
import cn.lili.modules.file.entity.dto.FileOwnerDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 文件管理业务层
 *
 * @author Chopper
 */
public interface FileService extends IService<File> {


    /**
     * 批量删除
     *
     * @param ids
     */
    void batchDelete(List<String> ids);
    /**
     * 根据文件夹ID批量删除
     *
     * @param directoryId 文件夹ID
     */
    void batchDeleteByDirectory(String directoryId);

    /**
     * 所有者批量删除
     *
     * @param ids      ID
     * @param authUser 操作者
     */
    void batchDelete(List<String> ids, AuthUser authUser);


    /**
     * 自定义搜索分页
     *

     * @param fileOwnerDTO 文件查询

     * @return
     */
    IPage<File> customerPage(FileOwnerDTO fileOwnerDTO);

    /**
     * 所属文件数据查询
     *
     * @param ownerDTO 文件查询
     * @return
     */
    IPage<File> customerPageOwner(FileOwnerDTO ownerDTO);

}