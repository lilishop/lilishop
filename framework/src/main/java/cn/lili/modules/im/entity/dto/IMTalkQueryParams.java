package cn.lili.modules.im.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.im.entity.dos.ImTalk;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author paulG
 * @since 2023/2/6
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IMTalkQueryParams {

    @ApiModelProperty("用户1 id")
    private String userId1;

    @ApiModelProperty("用户2 id")
    private String userId2;

    @ApiModelProperty("用户1 name")
    private String name1;

    @ApiModelProperty("用户2 name")
    private String name2;

    @ApiModelProperty("关键字")
    private String userName;


    public QueryWrapper<ImTalk> queryWrapper() {
        QueryWrapper<ImTalk> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(userId1), "user_id1", this.userId1);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(userId2), "user_id2", this.userId2);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(name1), "name1", this.name1);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(name2), "name2", this.name2);
        queryWrapper.nested(CharSequenceUtil.isNotEmpty(userName), i -> i.like("name1", userName).or().like("name2", userName));
        return queryWrapper;
    }
}
