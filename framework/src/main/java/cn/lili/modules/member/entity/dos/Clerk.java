package cn.lili.modules.member.entity.dos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.member.entity.dto.ClerkAddDTO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 店员Model
 *
 * @author wget
 * @title: Clerk
 * @projectName lilishop
 * @date 2021/12/28 7:39 下午
 */
@Data
@TableName("li_clerk")
@ApiModel(value = "店员")
@NoArgsConstructor
@AllArgsConstructor
public class Clerk extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "店员名称")
    private String clerkName;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "角色id集合")
    private String roleIds;

    @ApiModelProperty(value = "是否是店主", hidden = true)
    private Boolean shopkeeper = false;

    @ApiModelProperty(value = "是否是超级管理员 超级管理员/普通管理员")
    private Boolean isSuper = false;

    @ApiModelProperty(value = "状态 默认true正常 false禁用")
    private Boolean status = true;


    /**
     * 构建店员
     *
     * @param clerkAddDTO
     */
    public Clerk(ClerkAddDTO clerkAddDTO) {
        if (clerkAddDTO.getRoles()!=null && !clerkAddDTO.getRoles().isEmpty()) {
            this.roleIds = CharSequenceUtil.join(",", clerkAddDTO.getRoles());
        }
        this.memberId = clerkAddDTO.getMemberId();
        this.departmentId = clerkAddDTO.getDepartmentId();
        this.storeId = clerkAddDTO.getStoreId();
        this.clerkName = clerkAddDTO.getUsername();

    }


    public Clerk(Store store){
        this.memberId = store.getMemberId();
        this.storeId = store.getId();
        this.clerkName = store.getMemberName();
        this.setShopkeeper(true);
        this.setIsSuper(true);
        this.setStatus(true);
    }
}
