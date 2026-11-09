package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.dto.StoreEditDTO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺详细VO
 *
 * @author pikachu
 * @since 2020-03-09 21:53:20
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class StoreDetailVO extends StoreEditDTO {

    /**
     * 客户名称（商家登录会员账号），关联 li_store.member_name
     */
    @Schema(description = "客户名称")
    private String memberName;

    /**
     * 开店时间，关联 li_store.create_time，格式 yyyy-MM-dd HH:mm:ss（GMT+8）
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "开店时间")
    private Date createTime;

}
