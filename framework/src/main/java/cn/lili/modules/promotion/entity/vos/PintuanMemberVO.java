package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.member.entity.dos.Member;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 拼图会员视图对象
 *
 * @author paulG
 * @since 2021/3/3
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class PintuanMemberVO {

    @ApiModelProperty(value = "会员编号")
    private String memberId;

    @ApiModelProperty(value = "会员用户名")
    private String memberName;

    @ApiModelProperty(value = "会员头像")
    private String face;

    @ApiModelProperty(value = "昵称")
    private String nickName;

    @ApiModelProperty(value = "参团订单编号")
    private String orderSn;

    @ApiModelProperty(value = "已参团人数")
    private long groupedNum;

    @ApiModelProperty(value = "待参团人数")
    private long toBeGroupedNum;

    @ApiModelProperty(value = "成团人数")
    private long groupNum;

    public PintuanMemberVO(Member member) {
        this.memberId = member.getId();
        this.memberName = member.getUsername();
        this.face = member.getFace();
        this.nickName = member.getNickName();
    }
}
