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
     * 删除超过100条后的记录
     *
     * @param memberId 会员ID
     */

    @Delete("DELETE FROM li_foot_print WHERE id IN ("+
            "SELECT l2.id FROM (" +
            "SELECT l3.id FROM li_foot_print l3 WHERE l3.member_id=${memberId} ORDER BY id DESC LIMIT 100,100) l2)")
    void deleteLastFootPrint(String memberId);

}