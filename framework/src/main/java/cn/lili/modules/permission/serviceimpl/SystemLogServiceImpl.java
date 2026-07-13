package cn.lili.modules.permission.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.dos.SystemLog;
import cn.lili.modules.permission.entity.vo.SystemLogVO;
import cn.lili.modules.permission.mapper.SystemLogMapper;
import cn.lili.modules.permission.service.SystemLogService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 系统日志（MySQL）
 */
@Service
public class SystemLogServiceImpl implements SystemLogService {

    @Autowired
    private SystemLogMapper systemLogMapper;

    @Override
    public void saveLog(SystemLogVO systemLogVO) {
        if (systemLogVO == null) {
            return;
        }
        SystemLog entity = systemLogVO.toEntity();
        if (CharSequenceUtil.isEmpty(entity.getId())) {
            entity.setId(IdUtil.getSnowflakeNextIdStr());
        }
        if (entity.getCreateTime() == null) {
            entity.setCreateTime(System.currentTimeMillis());
        }
        systemLogMapper.insert(entity);
    }

    @Override
    public void deleteLog(List<String> id) {
        if (id == null || id.isEmpty()) {
            return;
        }
        systemLogMapper.deleteByIds(id);
    }

    @Override
    public void flushAll() {
        systemLogMapper.delete(Wrappers.emptyWrapper());
    }

    @Override
    public IPage<SystemLogVO> queryLog(String storeId, String operatorName, String key, SearchVO searchVo, PageVO pageVO) {
        pageVO.setNotConvert(true);
        LambdaQueryWrapper<SystemLog> wrapper = Wrappers.lambdaQuery();
        if (CharSequenceUtil.isNotEmpty(storeId)) {
            wrapper.eq(SystemLog::getStoreId, Long.parseLong(storeId));
        }
        if (CharSequenceUtil.isNotEmpty(operatorName)) {
            wrapper.like(SystemLog::getUsername, operatorName);
        }
        if (CharSequenceUtil.isNotEmpty(key)) {
            wrapper.and(w -> w.like(SystemLog::getRequestUrl, key)
                    .or().like(SystemLog::getRequestParam, key)
                    .or().like(SystemLog::getResponseBody, key)
                    .or().like(SystemLog::getName, key)
                    .or().like(SystemLog::getCustomerLog, key)
                    .or().like(SystemLog::getIpInfo, key));
        }
        if (searchVo != null && searchVo.getConvertStartDate() != null && searchVo.getConvertEndDate() != null) {
            wrapper.between(SystemLog::getCreateTime,
                    searchVo.getConvertStartDate().getTime(),
                    searchVo.getConvertEndDate().getTime());
        }
        if (CharSequenceUtil.isNotEmpty(pageVO.getSort()) && CharSequenceUtil.isNotEmpty(pageVO.getOrder())) {
            boolean asc = "asc".equalsIgnoreCase(pageVO.getOrder());
            switch (pageVO.getSort()) {
                case "createTime" -> wrapper.orderBy(true, asc, SystemLog::getCreateTime);
                case "costTime" -> wrapper.orderBy(true, asc, SystemLog::getCostTime);
                case "username" -> wrapper.orderBy(true, asc, SystemLog::getUsername);
                default -> wrapper.orderByDesc(SystemLog::getCreateTime);
            }
        } else {
            wrapper.orderByDesc(SystemLog::getCreateTime);
        }

        Page<SystemLog> page = new Page<>(pageVO.getPageNumber(), pageVO.getPageSize());
        IPage<SystemLog> result = systemLogMapper.selectPage(page, wrapper);
        Page<SystemLogVO> voPage = new Page<>(result.getCurrent(), result.getSize(), result.getTotal());
        voPage.setRecords(result.getRecords().stream().map(SystemLogVO::fromEntity).toList());
        return voPage;
    }
}
