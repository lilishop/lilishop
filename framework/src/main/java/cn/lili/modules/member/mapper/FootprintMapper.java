package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.FootPrint;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Delete;
import org.springframework.transaction.annotation.Transactional;

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
    @Transactional(rollbackFor = Exception.class)
    @Delete("DELETE li_foot_print " +
            "FROM li_foot_print " +
            "LEFT JOIN ( " +
            "  SELECT id " +
            "  FROM ( " +
            "    SELECT id " +
            "    FROM li_foot_print " +
            "    WHERE member_id = ${memberId} " +
            "    ORDER BY create_time DESC " +
            "    LIMIT 1 " +
            "  ) AS keep " +
            ") AS latest_footprints " +
            "ON li_foot_print.id = latest_footprints.id " +
            "WHERE li_foot_print.member_id = ${memberId} AND latest_footprints.id IS NULL; ")
    void deleteLastFootPrint(String memberId);

}