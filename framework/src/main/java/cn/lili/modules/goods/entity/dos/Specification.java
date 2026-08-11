package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

/**
 * 商品规格项
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_specification")
@Schema(description = "规格项")
@EqualsAndHashCode(callSuper = false)
public class Specification extends BaseIdEntity {

    private static final long serialVersionUID = 147792597901239486L;

    /**
     * 规格名称
     */
    @NotEmpty(message = "规格名称不能为空")
    @Size(max = 20, message = "规格名称不能超过20个字符")
    @Schema(description = "规格名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String specName;

    /**
     * 所属卖家 0属于平台
     * <p>
     * 店铺自定义规格暂时废弃 2021-06-23
     * 后续推出新配置方式
     */
    @Schema(hidden = true)
    private String storeId;

    /**
     * 规格值名字
     */
    @Schema(description = "规格值名字, 《,》分割")
    @Length(max = 255, message = "长度超出限制")
    private String specValue;


}