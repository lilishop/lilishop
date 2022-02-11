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
     * 所有者批量删除
     *
     * @param ids      ID
     * @param authUser 操作者
     */
    void batchDelete(List<String> ids, AuthUser authUser);


    /**
     * 自定义搜索分页
     *
     * @param file
     * @param searchVO
     * @param pageVo
     * @return
     */
    IPage<File> customerPage(File file, SearchVO searchVO, PageVO pageVo);

    /**
     * 所属文件数据查询
     *
     * @param file
     * @param searchVO
     * @param pageVo
     * @param ownerDTO
     * @return
     */
    IPage<File> customerPageOwner(FileOwnerDTO ownerDTO, File file, SearchVO searchVO, PageVO pageVo);

}