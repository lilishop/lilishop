package cn.lili.modules.permission.service;

import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.base.entity.systemlog.SystemLogVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

/**
 * 系统日志业务层
 *
 * @author Chopper
 * @date 2020/11/17 3:45 下午
 */
public interface SystemLogService {

    /**
     * 添加日志
     *
     * @param systemLogVO
     * @return
     */
    void saveLog(SystemLogVO systemLogVO);

    /**
     * 通过id删除日志
     *
     * @param id
     */
    void deleteLog(List<String> id);

    /**
     * 删除全部日志
     */
    void flushAll();

    /**
     * 分页搜索获取日志
     *
     * @param key
     * @param searchVo
     * @return
     */
    IPage<SystemLogVO> queryLog(String storeId, String operatorName, String key, SearchVO searchVo, PageVO pageVO);
}
