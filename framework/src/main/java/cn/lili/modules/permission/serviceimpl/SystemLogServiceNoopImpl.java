package cn.lili.modules.permission.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.vo.SystemLogVO;
import cn.lili.modules.permission.service.SystemLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 精简运行模式禁用 ES 后，系统日志功能降级为空实现，避免后台功能因日志索引缺失而无法启动。
 */
@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "false")
public class SystemLogServiceNoopImpl implements SystemLogService {

    @Override
    public void saveLog(SystemLogVO systemLogVO) {
        // 精简镜像不保留 ES 日志索引，日志写入直接忽略。
    }

    @Override
    public void deleteLog(List<String> id) {
        // 精简镜像不保留 ES 日志索引，删除操作直接忽略。
    }

    @Override
    public void flushAll() {
        // 精简镜像不保留 ES 日志索引，清空操作直接忽略。
    }

    @Override
    public IPage<SystemLogVO> queryLog(String storeId, String operatorName, String key, SearchVO searchVo, PageVO pageVO) {
        pageVO.setNotConvert(true);
        return new Page<>(pageVO.getPageNumber(), pageVO.getPageSize());
    }
}
