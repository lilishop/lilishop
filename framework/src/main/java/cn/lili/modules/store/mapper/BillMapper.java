package cn.lili.modules.store.mapper;


import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.vos.BillListVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 结算单数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 4:27 下午
 */
public interface BillMapper extends BaseMapper<Bill> {

    /**
     * 查询结算单分页
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 结算单分页
     */
    @Select("select b.id,b.sn,b.start_time,b.end_time,b.bill_status,b.store_name,b.bill_price,b.create_time from li_bill as b ${ew.customSqlSegment}")
    IPage<BillListVO> queryBillPage(IPage<BillListVO> page, @Param(Constants.WRAPPER) Wrapper<BillListVO> queryWrapper);



}