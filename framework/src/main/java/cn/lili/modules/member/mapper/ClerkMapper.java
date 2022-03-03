package cn.lili.modules.member.mapper;


import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.vo.ClerkVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 店员数据处理层
 *
 * @author wget
 * @title: ClerkMapper
 * @projectName lilishop
 * @date 2021/12/28 7:39 下午
 */
public interface ClerkMapper extends BaseMapper<Clerk> {

    /**
     * 查询店员分页数据
     * @param page 分页信息
     * @param ew 店铺ID
     * @return
     */
    @Select("select li_clerk.*,m.id,m.mobile as mobile from li_clerk inner join li_member as m on li_clerk.member_id = m.id ${ew.customSqlSegment}")
    IPage<ClerkVO> selectClerkPage(Page page, @Param(Constants.WRAPPER) QueryWrapper ew);


}