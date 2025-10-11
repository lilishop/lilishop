package cn.lili.modules.wallet.mapper;

import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.wallet.entity.dos.Recharge;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 预存款充值记录数据处理层
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
public interface RechargeMapper extends BaseMapper<Recharge> {


    /**
     * 获取会员预存款
     *
     * @return 会员预存款
     */
    @Select("SELECT COALESCE(SUM(recharge_money), 0) FROM li_recharge ${ew.customSqlSegment}")
    Double getRecharge(@Param(Constants.WRAPPER) Wrapper<Recharge> queryWrapper);

}