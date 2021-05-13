package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.vo.AfterSaleVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 售后数据处理层
 *
 * @author Chopper
 * @date 2020/11/17 7:34 下午
 */
public interface AfterSaleMapper extends BaseMapper<AfterSale> {

    @Select("SELECT * FROM li_after_sale ${ew.customSqlSegment}")
    IPage<AfterSaleVO> queryByParams(IPage<AfterSaleVO> page, @Param(Constants.WRAPPER) Wrapper<AfterSaleVO> queryWrapper);

    @Select("SELECT * FROM li_after_sale WHERE sn=#{sn}")
    AfterSaleVO getAfterSaleVO(String sn);
}