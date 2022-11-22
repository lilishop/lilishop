package cn.lili.im.entity;


import cn.lili.mybatis.BaseTenantEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @author Chopper
 */
@Data
@TableName("li_im_users")
@ApiModel(value = "Im消息")
public class ImUser extends BaseTenantEntity {

    private static final long serialVersionUID = 1L;
    /**
     * 头像
     */
    private String face;
    /**
     * 昵称
     */
    private String name;
}