package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 预约系统表单模板，对应 {@code li_appointment_system_form}。
 * 发品时把 schema 快照到商品扩展，买家按下单时表单填写。
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_system_form")
@Schema(description = "预约系统表单模板")
public class AppointmentSystemForm extends BaseEntity {

    @Schema(description = "店铺 ID")
    private String storeId;

    @Schema(description = "表单名称")
    private String formName;

    @Schema(description = "字段定义 JSON（snake_case field_type/field_key）")
    private String formSchema;

    @Schema(description = "ENABLED/DISABLED")
    private String status;
}
