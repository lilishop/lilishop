package cn.lili.modules.page.mapper;

import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.vos.PageDataListVO;
import cn.lili.modules.page.entity.vos.PageDataVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 楼层装修设置数据处理层
 *
 * @author paulGao
 * @date 2020/12/7 11:26
 */
public interface PageDataMapper extends BaseMapper<PageData> {

    @Select("SELECT page_data FROM li_page_data ${ew.customSqlSegment}")
    PageDataVO getPageData(@Param(Constants.WRAPPER) Wrapper<PageDataVO> queryWrapper);

    @Select("SELECT COUNT(id) FROM li_page_data ${ew.customSqlSegment}")
    Integer getPageDataNum(@Param(Constants.WRAPPER) Wrapper<Integer> queryWrapper);

    @Select("SELECT id,name,page_show FROM li_page_data ${ew.customSqlSegment}")
    IPage<PageDataListVO> getPageDataList(IPage<PageDataListVO> page, @Param(Constants.WRAPPER) Wrapper<PageDataListVO> queryWrapper);

}