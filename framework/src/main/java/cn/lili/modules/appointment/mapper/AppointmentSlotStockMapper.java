package cn.lili.modules.appointment.mapper;

import cn.lili.modules.appointment.entity.dos.AppointmentSlotStock;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

/**
 * 预约时段库存数据处理层
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
public interface AppointmentSlotStockMapper extends BaseMapper<AppointmentSlotStock> {

    @Select("SELECT * FROM li_appointment_slot_stock WHERE id = #{id} AND delete_flag = 0 FOR UPDATE")
    AppointmentSlotStock selectForUpdateById(@Param("id") String id);

    /**
     * 原子扣减时段库存 N；capacity=0 表示不限，仅记录占用。
     *
     * @return 影响行数，0 表示库存不足
     */
    @Update("UPDATE li_appointment_slot_stock SET available_qty = available_qty - #{qty}, version = version + 1 "
            + "WHERE id = #{id} AND delete_flag = 0 "
            + "AND (capacity = 0 OR available_qty >= #{qty})")
    int deductAvailableQty(@Param("id") String id, @Param("qty") int qty);

    @Update("UPDATE li_appointment_slot_stock SET available_qty = available_qty + #{qty}, version = version + 1 "
            + "WHERE id = #{id} AND delete_flag = 0")
    int restoreAvailableQty(@Param("id") String id, @Param("qty") int qty);
}
