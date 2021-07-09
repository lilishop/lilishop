package cn.lili.modules.permission.serviceimpl;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.vo.SystemLogVO;
import cn.lili.modules.permission.service.SystemLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.regex.Pattern;

/**
 * 系统日志
 *
 * @author Chopper
 * @date 2020/11/17 3:45 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SystemLogServiceImpl implements SystemLogService {

    @Autowired
    private MongoTemplate mongoTemplate;

    @Override
    public void saveLog(SystemLogVO systemLogVO) {
        mongoTemplate.save(systemLogVO);
    }

    @Override
    public void deleteLog(List<String> id) {
        mongoTemplate.remove(new Query().addCriteria(Criteria.where("id").is(id)), SystemLogVO.class);
    }

    @Override
    public void flushAll() {
        mongoTemplate.dropCollection(SystemLogVO.class);
    }

    @Override
    public IPage<SystemLogVO> queryLog(String storeId, String operatorName, String key, SearchVO searchVo, PageVO pageVO) {
        Query query = new Query();

        if (StringUtils.isNotEmpty(storeId)) {
            query.addCriteria(Criteria.where("storeId").is(storeId));
        }

        if (StringUtils.isNotEmpty(operatorName)) {
            query.addCriteria(Criteria.where("username").regex(Pattern.compile("^.*" + operatorName + ".*$", Pattern.CASE_INSENSITIVE)));
        }

        if (StringUtils.isNotEmpty(key)) {
            query.addCriteria(new Criteria().orOperator(
                    Criteria.where("requestUrl").regex(Pattern.compile("^.*" + key + ".*$", Pattern.CASE_INSENSITIVE)),
                    Criteria.where("requestParam").regex(Pattern.compile("^.*" + key + ".*$", Pattern.CASE_INSENSITIVE)),
                    Criteria.where("responseBody").regex(Pattern.compile("^.*" + key + ".*$", Pattern.CASE_INSENSITIVE)),
                    Criteria.where("name").regex(Pattern.compile("^.*" + key + ".*$", Pattern.CASE_INSENSITIVE))
            ));
        }
        //时间有效性判定
        if (searchVo.getConvertStartDate() != null && searchVo.getConvertEndDate() != null) {
            //大于方法
            Criteria gt = Criteria.where("createTime").gt(searchVo.getConvertStartDate());
            //小于方法
            Criteria lt = Criteria.where("createTime").lte(searchVo.getConvertEndDate());
            query.addCriteria(new Criteria().andOperator(gt, lt));

        }

        IPage<SystemLogVO> iPage = new Page<>();

        iPage.setTotal(mongoTemplate.count(query, SystemLogVO.class));
        query.with(PageRequest.of(pageVO.getMongoPageNumber(), pageVO.getPageSize()));

        query.with(Sort.by(Sort.Direction.DESC, "createTime"));

        List<SystemLogVO> systemLogVOS = mongoTemplate.find(query, SystemLogVO.class);
        iPage.setCurrent(pageVO.getPageNumber());
        iPage.setSize(pageVO.getPageSize());
        iPage.setRecords(systemLogVOS);
        return iPage;
    }

}
