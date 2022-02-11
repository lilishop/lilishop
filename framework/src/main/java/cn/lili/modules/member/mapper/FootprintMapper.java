package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.FootPrint;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Delete;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 浏览历史数据处理层
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
public interface FootprintMapper extends BaseMapper<FootPrint> {

    /**
     * 获取用户足迹的SkuId分页
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 用户足迹的SkuId分页
     */
    @Select("select sku_id from li_foot_print  ${ew.customSqlSegment} ")
    List<String> footprintSkuIdList(IPage<String> page, @Param(Constants.WRAPPER) Wrapper<FootPrint> queryWrapper);

    /**
     * 删除超过100条后的记录
     *
     * @param memberId 会员ID
     */
    @Delete("DELETE FROM li_foot_print l1 WHERE l1.id IN (" +
            "SELECT l2.id FROM (" +
            "SELECT l3.id FROM li_foot_print l3 WHERE l3.member_id=${memberId} ORDER BY id DESC LIMIT 100,100) l2)")
    void deleteLastFootPrint(String memberId);

}