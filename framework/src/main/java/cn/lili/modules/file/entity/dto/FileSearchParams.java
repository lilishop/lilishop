package cn.lili.modules.file.entity.dto;

import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.file.entity.File;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
public class FileSearchParams extends PageVO {

    @ApiModelProperty(value = "文件")
    private File file;
    @ApiModelProperty(value = "搜索VO")
    private SearchVO searchVO;
    @ApiModelProperty(value = "文件夹ID")
    private String fileDirectoryId;
}
