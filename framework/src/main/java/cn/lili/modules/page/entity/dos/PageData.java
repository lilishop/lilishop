package cn.lili.modules.page.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.modules.page.entity.enums.PageEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 页面数据DO
 *
 * @author Bulbasaur
 * @date 2020/12/10 17:42
 */
@Data
@Entity
@Table(name = "li_page_data")
@TableName("li_page_data")
@ApiModel(value = "页面数据DO")
@NoArgsConstructor
public class PageData extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "页面名称")

    private String name;

    @ApiModelProperty(value = "页面数据")
    private String pageData;

    /**
     * @see SwitchEnum
     */
    @ApiModelProperty(value = "页面开关状态", allowableValues = "OPEN,CLOSE")
    private String pageShow;

    /**
     * @see PageEnum
     */
    @ApiModelProperty(value = "页面类型", allowableValues = "INDEX,STORE,SPECIAL")
    private String pageType;

    /**
     * @see cn.lili.modules.base.entity.enums.ClientTypeEnum
     */
    @ApiModelProperty(value = "客户端类型", allowableValues = "PC,H5,WECHAT_MP,APP")
    private String pageClientType;

    @ApiModelProperty(value = "值")
    private String num;

    public PageData(String name, String pageClientType, String pageData) {
        this.pageClientType = pageClientType;
        this.pageData = pageData;
        this.num = UserContext.getCurrentUser().getStoreId();
        this.pageShow = SwitchEnum.CLOSE.name();
        this.pageType = PageEnum.STORE.name();
    }

}