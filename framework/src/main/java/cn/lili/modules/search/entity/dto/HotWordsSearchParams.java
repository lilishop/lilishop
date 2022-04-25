package cn.lili.modules.search.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.beans.BeanUtils;

import javax.validation.constraints.NotNull;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

/**
 * 商品查询条件
 *
 * @author pikachu
 * @since 2020-02-24 19:27:20
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class HotWordsSearchParams extends PageVO {

    private static final long serialVersionUID = 2544015852728566887L;

    @NotNull(message = "搜索热词不能为空")
    @ApiModelProperty(value = "热词")
    private String keywords;

    @ApiModelProperty(value = "快捷搜索", allowableValues = "TODAY, YESTERDAY, LAST_SEVEN, LAST_THIRTY")
    private String searchType;

    @ApiModelProperty(value = "类型：年（YEAR）、月（MONTH）")
    private String timeType;

    @ApiModelProperty(value = "年份")
    private Integer year;

    @ApiModelProperty(value = "月份")
    private Integer month;


    //临时参数 不作为前端传递
    private Date startTIme;

    private Date endTime;

    //搜索热词数量
    private Integer top = 50;

    public <T> QueryWrapper<T> queryWrapper() {
        //组织查询时间
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        StatisticsQueryParam statisticsQueryParam = new StatisticsQueryParam();
        BeanUtils.copyProperties(this, statisticsQueryParam);
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);

        //获取当前时间
        Calendar calendar = Calendar.getInstance();


        calendar.set(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH), 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        if (StringUtils.isNotEmpty(keywords)) {
            queryWrapper.like("keywords", keywords);
        }
        queryWrapper.between("create_time", dates[0], dates[1]);

        startTIme = dates[0];
        endTime = dates[1];

        return queryWrapper;
    }

}
