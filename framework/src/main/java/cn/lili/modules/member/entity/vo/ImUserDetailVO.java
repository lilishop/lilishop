package cn.lili.modules.member.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.member.entity.dos.Member;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * IM 用户信息（跨用户展示场景）
 * 只暴露昵称/头像等公开字段，避免泄露手机号、密码散列、钱包等敏感信息
 *
 * @author Bulbasaur
 */
@Data
@NoArgsConstructor
public class ImUserDetailVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "唯一标识")
    private String id;

    @Schema(description = "昵称")
    private String nickName;

    @Schema(description = "客户头像")
    private String face;

    public ImUserDetailVO(Member member) {
        BeanUtil.copyProperties(member, this);
    }
}
