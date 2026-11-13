package cn.lili.modules.store.support;

import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.mapper.BillMapper;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.stereotype.Component;

/**
 * 结算单基础查询，供结算单、商家流水和财务导出共用，避免查询方反向依赖结算业务服务。
 * 只读取已有结算单，不生成账单、计算金额或推进状态，沿用调用方的查询条件和事务。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class BillQuerySupport {

    private final BillMapper mapper;

    /**
     * 注入结算单存储，不依赖商家流水或财务导出服务。
     *
     * @param mapper 结算单存储
     */
    public BillQuerySupport(BillMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 按主键读取结算单，沿用存储层逻辑删除规则，不额外限制账单状态或店铺。
     *
     * @param id 结算单主键
     * @return 结算单，不存在时为null
     */
    public Bill getById(String id) {
        return mapper.selectById(id);
    }

    /**
     * 复用原有列表条件、分页初始化及展示字段查询，不额外改变店铺和时间范围过滤。
     *
     * @param searchParams 结算单筛选与分页参数
     * @return 结算单展示分页
     */
    public IPage<BillListVO> billPage(BillSearchParams searchParams) {
        QueryWrapper<BillListVO> queryWrapper = searchParams.queryWrapperBillList();
        return mapper.queryBillPage(PageUtil.initPage(searchParams), queryWrapper);
    }
}
