package cn.lili.modules.member.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 会员评价VO
 *
 * @author Bulbasaur
 * @since 2020/11/30 15:00
 */
@Data
@NoArgsConstructor
public class MemberEvaluationVO extends MemberEvaluation {

    private static final long serialVersionUID = 6696978796248845481L;

    @ApiModelProperty(value = "评论图片")
    private List<String> evaluationImages;

    @ApiModelProperty(value = "回复评论图片")
    private List<String> replyEvaluationImages;

    public MemberEvaluationVO(MemberEvaluation memberEvaluation) {
        BeanUtil.copyProperties(memberEvaluation, this);
    }
}
