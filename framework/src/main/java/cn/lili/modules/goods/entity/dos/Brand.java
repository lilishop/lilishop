package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

/**
 * 商品品牌
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_brand")
@Schema(description = "商品品牌")
@EqualsAndHashCode(callSuper = false)
public class Brand extends BaseEntity {


    private static final long serialVersionUID = -8236865838438521426L;

    @NotEmpty(message = "品牌名称不能为空")
    @Length(max = 20, message = "品牌名称应该小于20长度字符")
    @Schema(description = "品牌名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String name;

    @NotEmpty(message = "品牌图标不能为空")
    @Length(max = 255, message = "品牌图标地址长度超过255字符")
    @Schema(description = "品牌图标", requiredMode = Schema.RequiredMode.REQUIRED)
    private String logo;

}